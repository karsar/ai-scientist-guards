{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- src/Workspace.hs
module Workspace (
    PromptConfig(..),
    Hypothesis(..),
    DataContract(..),
    StatisticalTestSpec(..),
    loadHypotheses,
    loadPromptConfig,
    initializeWorkspace,
    readFileT,
    writeFileT,
    cleanupBaseWorkspace
) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import Text.Printf (printf)
import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import Control.Monad (when, zipWithM)

-- Define data types locally to avoid circular dependency
data Hypothesis = Hypothesis {
    hId :: Int,
    hName :: T.Text,
    hDescription :: T.Text,
    hContract :: DataContract,
    hTestSpec :: StatisticalTestSpec
} deriving (Show, Eq)

data DataContract = DataContract {
    explorationDataPath :: FilePath,
    validationDataPath :: FilePath
} deriving (Show, Eq)

data StatisticalTestSpec = 
    PairedTTest { repetitions :: Int, folds :: Int }
    | SimulationOnly
    deriving (Show, Eq)

-- Prompt Config
data PromptConfig = PromptConfig {
    systemMessage :: T.Text
} deriving (Show)

instance FromJSON PromptConfig where
    parseJSON = withObject "PromptConfig" $ \v -> PromptConfig <$> v .: "system_message"

-- Intermediate JSON structure for loading ideas
data IdeaJSON = IdeaJSON {
    name :: T.Text,
    description :: T.Text
} deriving (Show)

instance FromJSON IdeaJSON where
    parseJSON = withObject "IdeaJSON" $ \v -> IdeaJSON <$> v .: "name" <*> v .: "description"

-- Load hypotheses
loadHypotheses :: FilePath -> IO [Hypothesis]
loadHypotheses path = do
    content <- BS.readFile path
    case decodeStrict content of
        Nothing -> error $ "Failed to parse JSON file: " ++ path
        -- Use zipWithM to assign IDs (1-based) and attach contracts
        Just ideasJson -> zipWithM assembleHypothesis [1..] ideasJson

-- Define the contracts for the Case Study.
assembleHypothesis :: Int -> IdeaJSON -> IO Hypothesis
assembleHypothesis hIdVal idea = do
    -- Define contracts for the SVM optimization task
    -- NOTE: Ensure these data files exist in your project structure relative to the execution path.
    let contract = DataContract {
            explorationDataPath = "data/wine_exploration.csv",
            validationDataPath = "data/wine_validation.csv"
        }
    -- Define the required statistical test
    let testSpec = PairedTTest { repetitions = 3, folds = 10 }

    return Hypothesis {
        hId = hIdVal,
        hName = name idea,
        hDescription = description idea,
        hContract = contract,
        hTestSpec = testSpec
    }

loadPromptConfig :: FilePath -> IO PromptConfig
loadPromptConfig path = do
    content <- BS.readFile path
    case decodeStrict content of
        Nothing -> error $ "Failed to parse JSON file: " ++ path
        Just config -> return config

initializeWorkspace :: FilePath -> Hypothesis -> IO (Either String FilePath)
initializeWorkspace baseDir hypothesis = do
    -- Use a simple numeric ID-based directory name
    let workspaceDir = baseDir ++ "_" ++ show (hId hypothesis)
    
    result <- try $ do
        createDirectoryIfMissing True workspaceDir
        return workspaceDir
        
    case result of
        Left (e :: SomeException) -> return $ Left $ "Failed to setup workspace: " ++ show e
        Right dir -> return $ Right dir

readFileT :: FilePath -> IO T.Text
readFileT = TIO.readFile

writeFileT :: FilePath -> T.Text -> IO ()
writeFileT = TIO.writeFile

cleanupBaseWorkspace :: FilePath -> IO ()
cleanupBaseWorkspace dir = do
    exists <- doesDirectoryExist dir
    when exists $ do
        printf "Cleaning up workspace: %s\n" dir
        result <- try (removeDirectoryRecursive dir)
        case result of
            Left (e :: SomeException) -> printf "Warning: Failed to fully clean up workspace %s: %s\n" dir (show e)
            Right _ -> return ()