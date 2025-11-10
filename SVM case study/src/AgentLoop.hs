{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- src/AgentLoop.hs
module AgentLoop (
    runAgentExploration
) where

import qualified Data.Text as T
import Text.Printf (printf)
import Control.Monad.State
import System.FilePath ((</>))
import Data.List (foldl', isInfixOf)

import ResearchMonad
import Workspace (PromptConfig(..), Hypothesis(..), writeFileT, readFileT)
import LLMCoder
import Executor (executeHarnessScript)
import Protocol (StatisticalProtocol)

-- Configuration
maxIters :: Int
maxIters = 10 -- Increased from 5 to 10 max attempts by LLM to refactor the code.

-- The prompt template, updated for refactoring the baseline into the scaffold structure.
coderPromptTemplate :: T.Text
coderPromptTemplate =
    "{system_message}\n\n" <>
    "Your goal is to adapt the existing baseline experiment to test the following research idea: {title}.\n" <>
    "The proposed approach is: {idea}.\n\n" <>
    "A rigorous execution harness (`harness.py`) has been generated. This harness structurally enforces a strict Exploration/Validation split and a standardized statistical methodology.\n\n" <>
    "YOUR TASK IS TO REFACTOR THE BASELINE CODE INTO THE `implementation.py` FILE REQUIRED BY THE HARNESS.\n\n" <>
    "--- Baseline Code (`experiment.py`) ---\n" <>
    "{baseline_code}\n" <>
    "--- End Baseline Code ---\n\n" <>
    "You must implement the following functions in `implementation.py`:\n" <>
    "1. `get_baseline()`: Returns the baseline model artifact (e.g., sklearn pipeline), derived from the baseline code.\n" <>
    "2. `optimize(data)`: Takes a pandas DataFrame (exploration data). It should return the optimized model artifact incorporating the research idea.\n" <>
    "3. `evaluate_model(artifact, X_train, y_train, X_test, y_test)`: Evaluates the artifact within a single cross-validation fold (managed by the harness) and returns the accuracy score.\n\n" <>
    "CRITICAL CONSTRAINTS:\n" <>
    "- DO NOT load data yourself. Use the data provided to the functions.\n" <>
    "- DO NOT perform cross-validation or data splitting; the harness handles this.\n\n" <>
    "Provide the complete code for `implementation.py`."

-- | Runs the iterative process with execution error handling and retry logic
-- Takes the baseline path as an argument for the refactoring task.
runAgentExploration :: (MonadIO m, StatisticalProtocol s)
                    => LLMConfig
                    -> PromptConfig
                    -> FilePath -- Path to the baseline experiment.py
                    -> FilePath -- Scaffold directory
                    -> Hypothesis
                    -> Research s m Bool -- Returns True if successful
runAgentExploration llmConfig promptConfig baselinePath scaffoldDir hypothesis = do
    logMessage $ printf "Agent: Starting implementation generation for H%d" (hId hypothesis)

    -- Load the baseline code to include in the prompt
    baselineCode <- liftIO $ readFileT baselinePath

    let initialPrompt = formatPrompt promptConfig hypothesis baselineCode
    tryImplementWithRetry llmConfig scaffoldDir initialPrompt hypothesis 5 -- Increased from 3 to 5 retry cycles

-- | Try to implement and execute with retry on errors
tryImplementWithRetry :: (MonadIO m, StatisticalProtocol s) 
                      => LLMConfig -> FilePath -> T.Text -> Hypothesis -> Int -> Research s m Bool
tryImplementWithRetry llmConfig scaffoldDir prompt hypothesis attemptsLeft = do
    if attemptsLeft <= 0
        then do
            logMessage "Agent: Max retry attempts reached. Implementation failed."
            return False
        else do
            logMessage $ printf "Agent: Starting retry cycle %d/5" (6 - attemptsLeft) -- Fixed: now shows X/5 instead of hardcoded /3
            -- Generate implementation
            success <- tryImplement llmConfig scaffoldDir prompt maxIters
            if not success
                then do
                    logMessage $ printf "Agent: Implementation generation failed. Retries left: %d" (attemptsLeft - 1)
                    tryImplementWithRetry llmConfig scaffoldDir prompt hypothesis (attemptsLeft - 1)
                else do
                    -- Test the implementation by running a quick execution
                    testResult <- testImplementation scaffoldDir
                    case testResult of
                        Right _ -> do
                            logMessage "Agent: Implementation successful and executable."
                            return True
                        Left errorMsg -> do
                            logMessage $ printf "Agent: Implementation failed execution. Error: %s" errorMsg
                            logMessage $ printf "Agent: Retrying with error feedback. Retries left: %d" (attemptsLeft - 1)
                            
                            -- Generate error feedback prompt
                            let errorFeedbackPrompt = generateErrorFeedbackPrompt prompt errorMsg
                            tryImplementWithRetry llmConfig scaffoldDir errorFeedbackPrompt hypothesis (attemptsLeft - 1)

-- | Recursive function to handle LLM interaction.
tryImplement :: (MonadIO m, StatisticalProtocol s) => LLMConfig -> FilePath -> T.Text -> Int -> Research s m Bool
tryImplement _ _ _ 0 = do
    logMessage "Agent: Max LLM generation attempts reached."
    return False
tryImplement llmConfig scaffoldDir prompt itersLeft = do
    logMessage $ printf "Agent: Calling LLM (Generation attempt %d/10)..." (maxIters - itersLeft + 1) -- Fixed: now shows X/10 instead of hardcoded /5

    response <- liftIO $ runCoderAgent llmConfig prompt
    case response of
        Left err -> do
            logMessage $ printf "Agent: LLM API call failed: %s" err
            logMessage $ printf "Agent: Retrying LLM call. Attempts left: %d" (itersLeft - 1)
            tryImplement llmConfig scaffoldDir prompt (itersLeft - 1)
        Right llmOutput -> do
            case extractPythonCode llmOutput of
                Nothing -> do
                    logMessage "Agent: LLM did not return a valid Python code block. Retrying."
                    logMessage $ printf "Agent: LLM generation attempts left: %d" (itersLeft - 1)
                    let feedbackPrompt = generateFormatErrorPrompt prompt llmOutput
                    tryImplement llmConfig scaffoldDir feedbackPrompt (itersLeft - 1)
                Just code -> do
                    -- Write the generated code to implementation.py
                    let implementationPath = scaffoldDir </> "implementation.py"
                    liftIO $ writeFileT implementationPath code
                    logMessage "Agent: Successfully generated implementation.py."
                    return True


-- | Formats the prompt for the LLM.
-- Includes baselineCode.
formatPrompt :: PromptConfig -> Hypothesis -> T.Text -> T.Text
formatPrompt promptConfig hypothesis baselineCode =
    let replacements = [
            ("{system_message}", systemMessage promptConfig),
            ("{title}", hName hypothesis),
            ("{idea}", hDescription hypothesis),
            ("{baseline_code}", baselineCode) -- Add baseline code to the prompt
            ]
    in foldl' (\acc (placeholder, value) -> T.replace placeholder value acc) coderPromptTemplate replacements

-- | Prompt when the LLM fails to provide code in the correct format.
generateFormatErrorPrompt :: T.Text -> T.Text -> T.Text
generateFormatErrorPrompt _originalPrompt llmResponse =
    "The previous response did not contain a valid Python code block or failed to adhere to the required structure.\n\n" <>
    "--- Previous LLM Response ---\n" <>
    llmResponse <> "\n--- End Previous Response ---\n\n" <>
    "You MUST provide the complete code for `implementation.py` (including `optimize`, `get_baseline`, `evaluate_model`) wrapped in ```python ... ``` tags."

-- | Generate error feedback prompt for the LLM with specific sklearn guidance
generateErrorFeedbackPrompt :: T.Text -> String -> T.Text
generateErrorFeedbackPrompt originalPrompt errorMsg =
    let specificGuidance = if "RFE" `isInfixOf` errorMsg && "coef_" `isInfixOf` errorMsg
        then "SPECIFIC ISSUE DETECTED: You're using RFE (Recursive Feature Elimination) with a non-linear SVM kernel. RFE requires linear models that expose feature coefficients. SOLUTIONS: 1) Use linear kernel: kernel='linear', 2) Use SelectKBest instead of RFE, 3) Use tree-based feature selection, 4) Skip feature selection entirely and focus on other improvements like scaling/hyperparameters."
        else "Common sklearn issues: Check kernel compatibility, ensure proper imports, verify data types."
    in
    "The previous implementation failed with the following error:\n\n" <>
    "ERROR:\n" <> T.pack errorMsg <> "\n\n" <>
    T.pack specificGuidance <> "\n\n" <>
    "Please fix the error and provide a corrected implementation. " <>
    "Focus on making the implementation work first, then optimize performance.\n\n" <>
    "Original request:\n" <> originalPrompt

-- | Test the generated implementation by running a quick execution
testImplementation :: (MonadIO m, StatisticalProtocol s) => FilePath -> Research s m (Either String ())
testImplementation scaffoldDir = do
    let harnessPath = scaffoldDir </> "harness.py"
    result <- liftIO $ executeHarnessScript harnessPath
    case result of
        Left err -> return $ Left err
        Right _ -> return $ Right ()