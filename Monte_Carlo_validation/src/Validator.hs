-- Validator.hs
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Validator (
    runValidation,
    MonteCarloConfig(..)
) where

-- Changed: Selective import to avoid importing the 'discoveries' accessor, resolving shadowing.
import Lord (LordConfig, LordState, alphaOverall)
import ResearchMonad
import Simulator
-- Changed: Import instances only.
import Protocol ()
import Control.Monad (forM, replicateM)
import Data.List (partition, foldl')
import Text.Printf (printf)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import System.Random.MWC (createSystemRandom, uniformR)

-- (MonteCarloConfig, ValidationResult definitions)
data MonteCarloConfig = MonteCarloConfig {
    numRuns :: Int,
    verbose :: Bool
} deriving (Show)

data ValidationResult = ValidationResult {
    vTotalTests :: Double, vTotalDiscoveries :: Double, vTrueDiscoveries :: Double,
    vFalseDiscoveries :: Double, vFDR :: Double, vPower :: Double
} deriving (Show)

-- Added explicit type signatures
zeroResult :: ValidationResult
zeroResult = ValidationResult 0 0 0 0 0 0

combineResults :: ValidationResult -> ValidationResult -> ValidationResult
combineResults r1 r2 = ValidationResult {
    vTotalTests = vTotalTests r1 + vTotalTests r2,
    vTotalDiscoveries = vTotalDiscoveries r1 + vTotalDiscoveries r2,
    vTrueDiscoveries = vTrueDiscoveries r1 + vTrueDiscoveries r2,
    vFalseDiscoveries = vFalseDiscoveries r1 + vFalseDiscoveries r2,
    vFDR = vFDR r1 + vFDR r2,
    vPower = vPower r1 + vPower r2
}

scaleResult :: ValidationResult -> Double -> ValidationResult
scaleResult r scalar = ValidationResult {
    vTotalTests = vTotalTests r * scalar, vTotalDiscoveries = vTotalDiscoveries r * scalar,
    vTrueDiscoveries = vTrueDiscoveries r * scalar, vFalseDiscoveries = vFalseDiscoveries r * scalar,
    vFDR = vFDR r * scalar, vPower = vPower r * scalar
}

calculateFDR :: Double -> Double -> Double
calculateFDR falseDiscoveries totalDiscoveries
    | totalDiscoveries == 0 = 0.0
    | otherwise = falseDiscoveries / totalDiscoveries

calculatePower :: Double -> Double -> Double
calculatePower trueDiscoveries totalTrueEffects
    | totalTrueEffects == 0 = 0.0
    | otherwise = trueDiscoveries / totalTrueEffects

-- | Main validation function (Monte Carlo)
runValidation :: MonteCarloConfig -> SimulationConfig -> LordConfig -> IO ()
runValidation mcCfg simCfg lordCfg = do
    printf "--- Starting Monte Carlo Simulation: Necessity of FDR Control at Scale ---\n"
    
    -- Setup workspace for scaffolds
    let baseWorkspace = "./workspace_sim"
    removePathForcibly baseWorkspace
    createDirectoryIfMissing True baseWorkspace

    -- Run the simulations N times
    results <- replicateM (numRuns mcCfg) (runSingleSimulation baseWorkspace simCfg lordCfg mcCfg)
    
    -- Filter out failed monadic runs (Maybe ValidationResult)
    let successfulRuns = [(m, n) | (Just m, n) <- results]
    let failedRuns = numRuns mcCfg - length successfulRuns

    if null successfulRuns
    then putStrLn "ERROR: All simulation runs failed."
    else do
        -- Aggregate results
        let (monadicResults, naiveResults) = unzip successfulRuns
        let avgMonadicResult = averageResults monadicResults
        let avgNaiveResult = averageResults naiveResults
        
        -- We can use alphaOverall because we imported it explicitly
        let targetFDR = alphaOverall lordCfg

        -- Report Results
        putStrLn "\n================= Monte Carlo Summary (Averaged) =================="
        printf "Simulation Parameters: N=%d Runs=%d (Failed: %d), True Effects=%.1f%%, Power Param=%.2f\n" 
            (nHypotheses simCfg) (numRuns mcCfg) failedRuns (propTrueEffects simCfg * 100) (powerParam simCfg)
        
        printf "\nApproach: Monadic (LORD++), Target FDR=%.3f\n" targetFDR
        reportResult avgMonadicResult

        printf "\nApproach: Naive (Fixed Alpha=%.3f)\n" targetFDR
        reportResult avgNaiveResult
        
        -- Conclusion
        putStrLn "\n--- Conclusion ---"
        if vFDR avgNaiveResult > targetFDR && vFDR avgMonadicResult <= targetFDR + 0.01
        then putStrLn "SUCCESS: The simulation confirms the necessity of FDR control at scale. The monadic implementation correctly maintained the target FDR."
        else putStrLn "NOTE: The simulation did not clearly demonstrate the expected control."

-- Added explicit type signature
averageResults :: [ValidationResult] -> ValidationResult
averageResults results = 
    let sumResult = foldl' combineResults zeroResult results
        n = fromIntegral (length results)
    in scaleResult sumResult (1.0 / n)

reportResult :: ValidationResult -> IO ()
reportResult r = do
    printf "Avg Total Discoveries: %.2f\n" (vTotalDiscoveries r)
    printf "  Avg Power (Sensitivity): %.4f\n" (vPower r)
    printf "Avg Empirical FDR:     %.4f\n" (vFDR r)

-- | Run a single simulation instance. Returns (Maybe MonadicResult, NaiveResult)
runSingleSimulation :: FilePath -> SimulationConfig -> LordConfig -> MonteCarloConfig -> IO (Maybe ValidationResult, ValidationResult)
runSingleSimulation baseWorkspace simCfg lordCfg mcCfg = do
    -- Create a unique workspace for this run
    gen <- createSystemRandom
    runId <- uniformR (10000, 99999) gen :: IO Int
    let workspaceDir = baseWorkspace </> printf "Run_%d" runId
    createDirectoryIfMissing True workspaceDir

    -- 1. Generate the hypothesis stream
    indexedHypotheses <- generateHypotheses simCfg
    
    -- 2. Run the Monadic (Guarded) approach
    monadicResult <- runMonadicApproach workspaceDir lordCfg indexedHypotheses (verbose mcCfg)
    
    -- 3. Run the Naive (Fixed Alpha) approach
    let fixedAlpha = alphaOverall lordCfg
    naiveResult <- runNaiveApproach workspaceDir fixedAlpha indexedHypotheses
    
    return (monadicResult, naiveResult)

-- | Run the simulation using the generalized Research Monad. Returns Maybe ValidationResult on error.
runMonadicApproach :: FilePath -> LordConfig -> [(Int, (Hypothesis, GroundTruth))] -> Bool -> IO (Maybe ValidationResult)
runMonadicApproach workspaceDir lordCfg indexedHypotheses verboseFlag = do
    
    -- Define the research program
    -- Explicitly typing the program. LordState is available due to the selective import.
    let researchProgram :: Research LordState IO [Maybe Discovery]
        researchProgram = forM indexedHypotheses $ \(i, (h, _)) -> do
            let hWithId = h { hId = i }
            -- Pass the workspace directory to testHypothesis
            testHypothesis workspaceDir hWithId verboseFlag

    -- Run the monad (handles Either result from ExceptT)
    (result, _) <- runResearch lordCfg researchProgram
    
    case result of
        Left err -> do
            -- Log the error if the entire run fails (e.g., due to a protocol violation)
            printf "Monadic execution failed with Error: %s\n" (show err)
            return Nothing
        Right discoveries_maybe -> do
            -- The name 'discoveries' no longer shadows anything due to the selective import.
            let discoveries = [d | Just d <- discoveries_maybe]
            return $ Just $ analyzeDiscoveries indexedHypotheses (map dHypothesis discoveries)

-- | Run the simulation using a fixed alpha threshold (Naive approach)
runNaiveApproach :: FilePath -> Double -> [(Int, (Hypothesis, GroundTruth))] -> IO ValidationResult
runNaiveApproach workspaceDir fixedAlpha indexedHypotheses = do
    
    let checkDiscovery :: (Int, (Hypothesis, GroundTruth)) -> IO (Maybe Hypothesis)
        checkDiscovery (i, (h, _)) = do
            let hWithId = h { hId = i }
            
            -- Generate Scaffold (Naive approach still needs the execution environment)
            scaffoldResult <- (generateScaffold hWithId) workspaceDir hWithId
            
            case scaffoldResult of
                -- We treat scaffold errors as fatal here for simplicity in the naive runner.
                Left err -> error $ "Scaffold generation failed in Naive approach: " ++ show err
                Right scaffold -> do
                    -- Execute Scaffold and obtain P-value
                    p_value_maybe <- executeHarness scaffold
                    
                    case p_value_maybe of
                        Nothing -> return Nothing
                        Just p_value -> 
                            if p_value <= fixedAlpha
                            then return (Just hWithId)
                            else return Nothing

    -- Execute the tests sequentially
    discoveries_maybe <- mapM checkDiscovery indexedHypotheses
    let discoveries = [h | Just h <- discoveries_maybe]
    
    -- Analyze results
    return $ analyzeDiscoveries indexedHypotheses discoveries


-- | Analyze the discoveries against the ground truth
analyzeDiscoveries :: [(Int, (Hypothesis, GroundTruth))] -> [Hypothesis] -> ValidationResult
analyzeDiscoveries groundTruthData discoveries =
    let discoveryIds = map hId discoveries
        discoveredTruths = filter (\(i, _) -> i `elem` discoveryIds) groundTruthData
        (trueDs, falseDs) = partition (\(_, (_, truth)) -> truth == TrueEffect) discoveredTruths
        totalTrueEffects = fromIntegral $ length $ filter (\(_, (_, truth)) -> truth == TrueEffect) groundTruthData
        
        nTotal = fromIntegral (length groundTruthData)
        nDiscoveries = fromIntegral (length discoveries)
        nTrue = fromIntegral (length trueDs)
        nFalse = fromIntegral (length falseDs)

        fdr = calculateFDR nFalse nDiscoveries
        power = calculatePower nTrue totalTrueEffects
    in ValidationResult nTotal nDiscoveries nTrue nFalse fdr power
