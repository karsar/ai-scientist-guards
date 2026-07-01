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
            generatePythonHarness (hId hypothesis) scaffold
            return scaffold
        Right actualDir -> do
            let scaffold = Scaffold {
                scaffoldPath = actualDir,
                contract = hContract hypothesis,
                testSpec = hTestSpec hypothesis
            }

            -- Generate the Python harness file (harness.py)
            generatePythonHarness (hId hypothesis) scaffold
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


-- | Generates the Python harness file (harness.py): disjoint per-hypothesis
--   validation split (H3) and a paired permutation test on per-example losses (H1).
generatePythonHarness :: (MonadIO m, StatisticalProtocol s) => Int -> Scaffold -> Research s m ()
generatePythonHarness hypId scaffold = do
    let path = scaffoldPath scaffold
    let harnessPath = path </> "harness.py"

    let permCode = generatePermutationTestCode (testSpec scaffold)

    let harnessCode = T.unlines [
            "# harness.py (Generated by Haskell Orchestrator)",
            "import implementation",
            "import pandas as pd",
            "import numpy as np",
            "import sys, os",
            "",
            T.pack $ printf "HYP_ID = %d  # this hypothesis's disjoint validation split (H3)" hypId,
            "",
            "# === Locate the data directory portably (walk up from this file) ===",
            "def _find_data_dir():",
            "    d = os.path.dirname(os.path.abspath(__file__))",
            "    for _ in range(6):",
            "        cand = os.path.join(d, \"data\")",
            "        if os.path.exists(os.path.join(cand, \"wine_exploration.csv\")):",
            "            return cand",
            "        d = os.path.dirname(d)",
            "    print(\"Error: data/ with wine_exploration.csv not found\"); sys.exit(1)",
            "",
            "DATA_DIR = _find_data_dir()",
            "EXPLORATION_DATA_PATH = os.path.join(DATA_DIR, \"wine_exploration.csv\")",
            "VALIDATION_DATA_PATH = os.path.join(DATA_DIR, f\"wine_validation_H{HYP_ID}.csv\")",
            "",
            "def load_data(path):",
            "    try:",
            "        return pd.read_csv(path)",
            "    except FileNotFoundError as e:",
            "        print(f\"Error loading data: {e}. Check path: {path}\"); sys.exit(1)",
            "",
            "# === Statistical test: paired sign-flip permutation on held-out",
            "# === per-example losses (super-uniform nulls, H1) ===",
            permCode,
            "",
            "# === Execution Phases ===",
            "def run_exploration():",
            "    print(\"--- Harness: Running Exploration Phase ---\")",
            "    data = load_data(EXPLORATION_DATA_PATH)",
            "    # Only exploration data is passed to the LLM-generated code.",
            "    artifact = implementation.optimize(data.copy())",
            "    return artifact",
            "",
            "def run_validation(optimized, baseline):",
            "    print(f\"--- Harness: Validation on disjoint split H{HYP_ID} ---\")",
            "    expl = load_data(EXPLORATION_DATA_PATH)",
            "    val = load_data(VALIDATION_DATA_PATH)",
            "    return permutation_pvalue(optimized, baseline, expl, val)",
            "",
            "# === Main Execution Flow: Exploration -> Validation ===",
            "if __name__ == \"__main__\":",
            "    baseline_artifact = implementation.get_baseline()",
            "    optimized_artifact = run_exploration()",
            "    p_value = run_validation(optimized_artifact, baseline_artifact)",
            "    print(f\"FINAL_P_VALUE:{p_value}\")"
            ]

    liftIO $ writeFileT harnessPath harnessCode
    logMessage $ printf "Generated harness.py at %s" harnessPath

-- | Generates the Python permutation-test code based on the spec.
generatePermutationTestCode :: StatisticalTestSpec -> T.Text
generatePermutationTestCode (PermutationTest nPerm) = T.unlines [
        T.pack $ printf "N_PERM = %d" nPerm,
        "from sklearn.base import clone",
        "",
        "def _per_example_loss(model, X, y):",
        "    return (model.predict(X) != y).astype(float)",
        "",
        "def permutation_pvalue(optimized, baseline, expl, val, seed=0):",
        "    # optimized is already fit on exploration; fit a fresh baseline on the same data.",
        "    Xe, ye = expl.drop('target', axis=1).values, expl['target'].values",
        "    Xv, yv = val.drop('target', axis=1).values, val['target'].values",
        "    # Fit fresh clones of both on exploration (robust whether or not",
        "    # optimize() already returned a fitted estimator).",
        "    opt = clone(optimized); opt.fit(Xe, ye)",
        "    base = clone(baseline); base.fit(Xe, ye)",
        "    loss_opt = _per_example_loss(opt, Xv, yv)",
        "    loss_base = _per_example_loss(base, Xv, yv)",
        "    d = loss_base - loss_opt            # > 0 favors the optimized model",
        "    n = d.shape[0]; obs = float(d.sum())",
        "    if n == 0:",
        "        return 1.0",
        "    rng = np.random.default_rng(seed)",
        "    if n <= 22:                          # exact enumeration of sign flips",
        "        idx = np.arange(1 << n)",
        "        signs = ((idx[:, None] >> np.arange(n)) & 1).astype(float) * 2.0 - 1.0",
        "        return float(np.mean(signs @ d >= obs - 1e-12))",
        "    count, drawn = 0, 0                  # Monte-Carlo with +1/+1 correction",
        "    while drawn < N_PERM:",
        "        b = min(4096, N_PERM - drawn)",
        "        flips = rng.integers(0, 2, size=(b, n)).astype(float) * 2.0 - 1.0",
        "        count += int(np.sum(flips @ d >= obs - 1e-12))",
        "        drawn += b",
        "    return float((count + 1) / (N_PERM + 1))"
        ]
generatePermutationTestCode SimulationOnly = "def permutation_pvalue(optimized, baseline, expl, val): return 0.01  # Simulation"