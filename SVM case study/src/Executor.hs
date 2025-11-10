{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- src/Executor.hs
module Executor (
    executeHarnessScript
) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Text.Printf (printf)
import Control.Exception (try, SomeException)
import System.FilePath (takeDirectory)
import Data.List (find)
import Text.Read (readMaybe)

data ExecutionResult = ExecutionResult {
    exitCode :: Int,
    stdout :: String,
    stderr :: String,
    pValue :: Maybe Double
} deriving (Show)

executeHarnessScript :: FilePath -> IO (Either String Double)
executeHarnessScript scriptPath = do
    let command = "python3"
    let args = [scriptPath]

    printf "Executor: Running harness script: %s\n" scriptPath

    (exitCode, stdout, stderr) <- readProcessWithExitCode command args ""

    printf "--- STDOUT ---\n%s\n--- END STDOUT ---\n" stdout
    printf "--- STDERR ---\n%s\n--- END STDERR ---\n" stderr

    case exitCode of
        ExitSuccess -> do
            case extractPValue (T.pack stdout) of
                Just pVal -> return $ Right pVal
                Nothing -> return $ Left $ "Failed to extract P-value from output: " ++ stdout
        ExitFailure code -> return $ Left $ formatExecutionError code stdout stderr

formatExecutionError :: Int -> String -> String -> String
formatExecutionError code stdout stderr = 
    "Harness exited with code " ++ show code ++ ".\n" ++
    "STDOUT:\n" ++ stdout ++ "\n" ++
    "STDERR:\n" ++ stderr

-- | Extracts the P-value from the harness output (looks for FINAL_P_VALUE:...)
extractPValue :: T.Text -> Maybe Double
extractPValue output =
    let prefix = "FINAL_P_VALUE:"
        linesList = T.lines output
        match = find (T.isPrefixOf prefix) linesList
    in case match of
        Nothing -> Nothing
        Just line ->
            let valueStr = T.strip $ T.drop (T.length prefix) line
            -- Use readMaybe for safe conversion to Double
            in readMaybe (T.unpack valueStr)