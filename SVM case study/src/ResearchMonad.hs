{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- src/ResearchMonad.hs
module ResearchMonad (
    Research,
    ResearchError(..),
    Discovery(..),
    TestResult(..),
    Scaffold(..),
    runResearch,
    logMessage,
    testHypothesis,
    GenerateScaffoldFn,
    RunLLMImplementationFn,
    ExecuteHarnessFn,
    processTest
) where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T
import Text.Printf (printf)
import qualified Data.Vector.Unboxed as UV
import Workspace (Hypothesis(..), DataContract(..), StatisticalTestSpec(..))
import Protocol (StatisticalProtocol(..), ProtocolError(..))

-- Core Monad Definitions
data ResearchError = ResearchError String
    deriving (Show, Eq)

-- Discovery data type to represent the outcome of a hypothesis test
data Discovery = Discovery {
    dHypothesis :: Hypothesis,
    dPValue :: Double,
    dAlphaT :: Double,
    dTestNumber :: Int
} deriving (Show, Eq)

-- Test result data type for tracking both discoveries and non-discoveries
data TestResult = TestResult {
    trHypothesisId :: Int,
    trName :: T.Text,
    trPValue :: Maybe Double,
    trAlphaT :: Maybe Double,
    trWasDiscovery :: Bool
} deriving (Show, Eq)

-- Scaffolding Definitions
data Scaffold = Scaffold {
    scaffoldPath :: FilePath,
    contract :: DataContract,
    testSpec :: StatisticalTestSpec
} deriving (Show, Eq)

-- The Generalized Research Monad
type Research s m a = ExceptT ResearchError (StateT s m) a

-- Function types for dependency injection of scaffolding implementations
type GenerateScaffoldFn s m = Hypothesis -> FilePath -> Research s m Scaffold
type RunLLMImplementationFn s m = Scaffold -> Hypothesis -> Research s m ()
type ExecuteHarnessFn s m = Scaffold -> Research s m (Maybe Double)

-- Core Operation: testHypothesis (Parameterized)
-- | Generic test hypothesis function that works with any statistical protocol
testHypothesis :: (MonadIO m, StatisticalProtocol s)
               => FilePath  -- Workspace base directory
               -> Hypothesis 
               -> Bool  -- Whether to use scaffolding
               -> GenerateScaffoldFn s m
               -> RunLLMImplementationFn s m  
               -> ExecuteHarnessFn s m
               -> Research s m (Maybe TestResult)
testHypothesis workspaceBase hypothesis useScaffolding genScaffold runLLM execHarness = do
    logMessage $ printf "\nH%d: Generating Scaffold..." (hId hypothesis)
    
    if useScaffolding
        then do
            -- Generate scaffold (this will handle workspace setup internally)
            scaffold <- genScaffold hypothesis workspaceBase
            
            logMessage $ printf "H%d: Starting LLM Implementation/Exploration..." (hId hypothesis)
            
            -- Run LLM implementation
            runLLM scaffold hypothesis
            
            logMessage $ printf "H%d: Executing Harness (Validation)..." (hId hypothesis)
            
            -- Execute harness and get P-value
            pValueMaybe <- execHarness scaffold
            
            case pValueMaybe of
                Nothing -> do
                    logMessage $ printf "H%d: Execution failed or no result produced." (hId hypothesis)
                    return Nothing
                Just pValue -> do
                    -- Process the test result using the statistical protocol
                    discoveryMaybe <- processTest hypothesis pValue
                    case discoveryMaybe of
                        Nothing -> do
                            -- Get alpha_t for reporting even though it's not a discovery
                            currentState <- get
                            let (_, alpha_t, _) = advanceState pValue currentState
                            logMessage $ printf "Tested H%d (%s): P=%.5f, Discovery=False" 
                                (hId hypothesis) (T.unpack $ hName hypothesis) pValue
                            return $ Just $ TestResult (hId hypothesis) (hName hypothesis) (Just pValue) (Just alpha_t) False
                        Just discovery -> do
                            logMessage $ printf "Tested H%d (%s): P=%.5f, Alpha_t=%.5f, Discovery=True" 
                                (hId hypothesis) (T.unpack $ hName hypothesis) (dPValue discovery) (dAlphaT discovery)
                            return $ Just $ TestResult (hId hypothesis) (hName hypothesis) (Just $ dPValue discovery) (Just $ dAlphaT discovery) True
        else do
            -- Direct execution without scaffolding (legacy mode)
            logMessage "Direct execution mode not implemented"
            return Nothing

-- | Process a test result using the current statistical protocol
processTest :: (MonadIO m, StatisticalProtocol s) 
            => Hypothesis -> Double -> Research s m (Maybe Discovery)
processTest hypothesis pValue = do
    currentState <- get
    -- Use the StatisticalProtocol's advanceState method
    let (isDiscovery, alpha_t, newState) = advanceState pValue currentState
    
    -- Debug logging
    logMessage $ printf "ProcessTest Debug: P=%.5f, Alpha_t=%.5f, IsDiscovery=%s" 
        pValue alpha_t (show isDiscovery)
    
    if isDiscovery
        then do
            -- Use hypothesis ID as test number for now (simple but functional)
            let testNum = hId hypothesis
            let discovery = Discovery {
                    dHypothesis = hypothesis,
                    dPValue = pValue,
                    dAlphaT = alpha_t,
                    dTestNumber = testNum
                }
            put newState
            logMessage $ printf "Discovery recorded: P=%.5f, Alpha_t=%.5f, Test=%d" pValue alpha_t testNum
            return (Just discovery)
        else do
            put newState
            logMessage $ printf "No discovery: P=%.5f > Alpha_t=%.5f" pValue alpha_t
            return Nothing

-- Helper function to get current test number from state (simplified)
getCurrentTestNumber :: StatisticalProtocol s => s -> Int
getCurrentTestNumber _ = 1  -- Simplified - use a constant for now

-- Runner function
runResearch :: (Monad m, StatisticalProtocol s) => Config s -> Research s m a -> m (Either ResearchError a, s)
runResearch config program = do
    case initializeState config of
        Left initError -> error $ "Failed to initialize protocol state: " ++ show initError
        Right initialState -> do
            (result, finalState) <- runStateT (runExceptT program) initialState
            return (result, finalState)

-- Helper function for logging.
logMessage :: MonadIO m => String -> Research s m ()
logMessage msg = liftIO $ putStrLn msg