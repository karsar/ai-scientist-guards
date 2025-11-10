{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- src/Scaffold.hs
module Scaffold (
    CaseStudyContext(..),
    initializeScaffoldRunner
) where

import ResearchMonad
import Workspace (PromptConfig(..), Hypothesis(..), StatisticalTestSpec(..), DataContract(..), initializeWorkspace, writeFileT)
import LLMCoder (LLMConfig(..))
import AgentLoop (runAgentExploration)
import Executor (executeHarnessScript)
import Protocol (StatisticalProtocol)
-- Specialized for the Case Study
import Lord (LordState)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (throwError)
import Text.Printf (printf)
import System.FilePath ((</>))
import qualified Data.Text as T
import System.Directory (makeAbsolute, createDirectoryIfMissing)

-- Context holding configurations needed for the Case Study scaffolding
data CaseStudyContext = CaseStudyContext {
    ctxLLMConfig :: LLMConfig,
    ctxPromptConfig :: PromptConfig,
    ctxBaselinePath :: FilePath -- Path to the baseline experiment.py
}

-- Initializes the scaffolding functions, capturing the necessary context.
-- Returns the three functions required by the generalized testHypothesis.
initializeScaffoldRunner :: CaseStudyContext -> (
        GenerateScaffoldFn LordState IO,
        RunLLMImplementationFn LordState IO,
        ExecuteHarnessFn LordState IO
    )
initializeScaffoldRunner context = (
        \h wp -> generateScaffoldImpl context h wp,
        \s h -> runLLMImplementationImpl context s h,
        \s -> executeHarnessImpl s
    )

-- ============================================================================
-- Implementation Details
-- ============================================================================

-- | Implements GenerateScaffoldFn: Creates the directory and the Python harness.
generateScaffoldImpl :: (MonadIO m, StatisticalProtocol s) => CaseStudyContext -> Hypothesis -> FilePath -> Research s m Scaffold
generateScaffoldImpl context hypothesis workspaceDir = do
    -- Create a specific directory for this scaffold instance
    let scaffoldDir = workspaceDir </> printf "H%04d_Scaffold" (hId hypothesis)

    -- Initialize the directory structure
    initResult <- liftIO $ initializeWorkspace scaffoldDir hypothesis
    case initResult of
        Left err -> do
            logMessage $ printf "Failed to initialize workspace: %s" err
            -- Instead of throwing error, let's try to create the directory manually
            liftIO $ System.Directory.createDirectoryIfMissing True scaffoldDir
            let scaffold = Scaffold {
                scaffoldPath = scaffoldDir,
                contract = hContract hypothesis,
                testSpec = hTestSpec hypothesis
            }
            generatePythonHarness scaffold
            return scaffold
        Right actualDir -> do
            let scaffold = Scaffold {
                scaffoldPath = actualDir,
                contract = hContract hypothesis,
                testSpec = hTestSpec hypothesis
            }

            -- Generate the Python harness file (harness.py)
            generatePythonHarness scaffold
            return scaffold

-- | Implements RunLLMImplementationFn: Calls the AgentLoop to generate implementation.py.
runLLMImplementationImpl :: (MonadIO m, StatisticalProtocol s) => CaseStudyContext -> Scaffold -> Hypothesis -> Research s m ()
runLLMImplementationImpl context scaffold hypothesis = do
    logMessage $ printf "--- Starting Agent Exploration within Scaffold: %s ---" (scaffoldPath scaffold)

    let llmConfig = ctxLLMConfig context
    let promptConfig = ctxPromptConfig context
    let baselinePath = ctxBaselinePath context

    -- Run the exploration phase with retry logic
    success <- runAgentExploration llmConfig promptConfig baselinePath (scaffoldPath scaffold) hypothesis
    
    if success
        then logMessage "--- Agent Exploration Phase Complete (Success) ---"
        else logMessage "--- Agent Exploration Phase Complete (Failed after retries) ---"

-- | Implements ExecuteHarnessFn: Executes harness.py and captures the P-value.
executeHarnessImpl :: (MonadIO m, StatisticalProtocol s) => Scaffold -> Research s m (Maybe Double)
executeHarnessImpl scaffold = do
    logMessage "\n--- Executing Scaffold Harness (Validation) ---"
    let harnessScriptPath = scaffoldPath scaffold </> "harness.py"
    
    -- Convert to absolute path to avoid path resolution issues
    absHarnessPath <- liftIO $ makeAbsolute harnessScriptPath
    
    -- Execute the generated Python harness script
    outcome <- liftIO $ executeHarnessScript absHarnessPath
    case outcome of
        Left err -> do
            logMessage $ printf "Harness execution failed: %s" err
            -- Returning Nothing means the hypothesis is rejected but the process continues.
            return Nothing
        Right pValue -> do
            logMessage $ printf "Harness execution successful. P-Value: %.5f" pValue
            return (Just pValue)


-- | Generates the Python harness file (harness.py) - Enforces the Standardized Methodology
generatePythonHarness :: (MonadIO m, StatisticalProtocol s) => Scaffold -> Research s m ()
generatePythonHarness scaffold = do
    let path = scaffoldPath scaffold
    let harnessPath = path </> "harness.py"

    -- Generate the Python code that implements the statistical test.
    let tTestCode = generatePairedTTestCode (testSpec scaffold)

    -- Ensure data paths are absolute for robustness in the Python environment
    absExplorePath <- liftIO $ makeAbsolute (explorationDataPath $ contract scaffold)
    absValidatePath <- liftIO $ makeAbsolute (validationDataPath $ contract scaffold)


    let harnessCode = T.unlines [
            "# harness.py (Generated by Haskell Orchestrator)",
            "import implementation",
            "import pandas as pd",
            "import numpy as np",
            "from scipy import stats",
            "import sys, os",
            "",
            "# === Data Contract Enforcement ===",
            T.pack $ printf "EXPLORATION_DATA_PATH = \"%s\"" absExplorePath,
            T.pack $ printf "VALIDATION_DATA_PATH = \"%s\"" absValidatePath,
            "",
            "# Load data within the harness to ensure separation",
            "def load_data(path):",
            "    try:",
            "        return pd.read_csv(path)",
            "    except FileNotFoundError as e:",
            "        print(f\"Error loading data: {e}. Check path: {path}\")",
            "        sys.exit(1)",
            "",
            "# === Statistical Test Implementation (Verified Standardized Methodology) ===",
            tTestCode,
            "",
            "# === Execution Phases ===",
            "def run_exploration():",
            "    print(\"--- Harness: Running Exploration Phase ---\")",
            "    data = load_data(EXPLORATION_DATA_PATH)",
            "    # CRITICAL: Only exploration data is passed to the LLM-generated code.",
            "    # We pass a copy to prevent accidental modification of the master data.",
            "    artifact = implementation.optimize(data.copy())",
            "    return artifact",
            "",
            "def run_validation(artifact, baseline_artifact):",
            "    print(\"--- Harness: Running Validation Phase ---\")",
            "    data = load_data(VALIDATION_DATA_PATH)",
            "    # Use the harness-defined statistical test.",
            "    p_value = execute_paired_ttest(data.copy(), artifact, baseline_artifact)",
            "    return p_value",
            "",
            "# === Main Execution Flow ===",
            "# The harness executes the full cycle: Exploration -> Validation",
            "if __name__ == \"__main__\":",
            "    # 1. Baseline Definition (LLM provides this via refactoring)",
            "    baseline_artifact = implementation.get_baseline()",
            "",
            "    # 2. Exploration Phase (LLM Optimization/Refactoring)",
            "    optimized_artifact = run_exploration()",
            "",
            "    # 3. Validation Phase (Statistical Test)",
            "    p_value = run_validation(optimized_artifact, baseline_artifact)",
            "",
            "    # Output the P-value for the orchestrator to capture",
            "    print(f\"FINAL_P_VALUE:{p_value}\")"
            ]

    liftIO $ writeFileT harnessPath harnessCode
    logMessage $ printf "Generated harness.py at %s" harnessPath

-- | Generates the Python code for the paired t-test based on the spec.
generatePairedTTestCode :: StatisticalTestSpec -> T.Text
generatePairedTTestCode (PairedTTest reps folds) = T.unlines [
        T.pack $ printf "N_REPEATS = %d" reps,
        T.pack $ printf "N_FOLDS = %d" folds,
        "from sklearn.model_selection import RepeatedKFold",
        "",
        "def execute_paired_ttest(data, artifact, baseline_artifact):",
        "    print(f\"Executing Paired T-Test ({N_REPEATS} repeats, {N_FOLDS} folds)\")",
        "    # Fixed random state for reproducibility of the validation test",
        "    rkf = RepeatedKFold(n_splits=N_FOLDS, n_repeats=N_REPEATS, random_state=42)",
        "",
        "    scores_A = []",
        "    scores_B = []",
        "",
        "    # Assuming 'target' column is the label. This might need generalization.",
        "    if 'target' not in data.columns:",
        "        print(\"Error: 'target' column not found in validation data.\")",
        "        sys.exit(1)",
        "    X = data.drop('target', axis=1)",
        "    y = data['target']",
        "",
        "    for train_index, test_index in rkf.split(X):",
        "        X_train, X_test = X.iloc[train_index], X.iloc[test_index]",
        "        y_train, y_test = y.iloc[train_index], y.iloc[test_index]",
        "",
        "        # LLM Implementation provides the evaluation logic for a single fold",
        "        score_A = implementation.evaluate_model(artifact, X_train, y_train, X_test, y_test)",
        "        score_B = implementation.evaluate_model(baseline_artifact, X_train, y_train, X_test, y_test)",
        "",
        "        scores_A.append(score_A)",
        "        scores_B.append(score_B)",
        "",
        "    # Perform the actual t-test using scipy",
        "    if np.all(np.array(scores_A) == np.array(scores_B)):",
        "        return 1.0 # If scores are identical, no significant difference",
        "",
        "    t_stat, p_value = stats.ttest_rel(scores_A, scores_B)",
        "",
        "    # We use a one-sided test (Optimized > Baseline)",
        "    if np.mean(scores_A) <= np.mean(scores_B):",
        "        # If mean is not better, p-value is high (not significant in the desired direction)",
        "        return 1.0",
        "    else:",
        "        # For a one-sided test (A > B), we divide the two-sided p-value by 2.",
        "        return p_value / 2.0"
        ]
generatePairedTTestCode SimulationOnly = "def execute_paired_ttest(data, artifact, baseline_artifact): return 0.01 # Simulation"