{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- app/Main.hs (Case Study - Updated for Scaffolding and Baseline)
module Main where

import Lord
import ResearchMonad
import Workspace
import LLMCoder
import Scaffold
import Protocol ()

import qualified Data.Text as T
import Control.Monad (forM, forM_)
import Text.Printf (printf)
import Control.Exception (try, SomeException)

main :: IO ()
main = do
    printf "=== AI-Scientist Case Study (SVM Optimization with Scaffolding) ===\n\n"

    -- Define workspace and baseline
    let workspaceBase = "./workspace_casestudy"
    -- Define the path to the user-provided baseline script
    let baselineScriptPath = "baseline/experiment.py" 
    cleanupBaseWorkspace workspaceBase

    result <- try $ do
        -- 1. Load Configurations
        llmConfigMaybe <- getLLMConfig
        case llmConfigMaybe of
            Nothing -> error "Failed to initialize LLM Configuration (Check OPENAI_API_KEY env var)."
            Just llmConfig -> do
                -- Load hypotheses (now includes contracts)
                hypotheses <- loadHypotheses "seed_ideas.json"
                promptConfig <- loadPromptConfig "prompt.json"
                printf "Loaded %d hypotheses.\n\n" (length hypotheses)

                -- 2. Initialize Context and Scaffold Runner
                -- Pass the baseline path to the context
                let context = CaseStudyContext llmConfig promptConfig baselineScriptPath

                -- 3. Define LORD++ Configuration
                let targetFDR = 0.05
                let numHypos = length hypotheses
                let lordConfig = LordConfig {
                        alphaOverall = targetFDR,
                        w0 = 0.1 * targetFDR,
                        maxHypotheses = numHypos
                    }

                -- 4. Define the Research Program
                let researchProgram = runCaseStudy context workspaceBase hypotheses

                -- 5. Execute the Research Monad (Specialized to LordState and IO)
                (runResult, finalState) <- runResearch lordConfig researchProgram

                -- 6. Process Results
                case runResult of
                    Left err -> printf "\nResearch execution halted due to error: %s\n" (show err)
                    Right results -> do
                        printf "\n\n================= Final Results =================\n"
                        printf "Target FDR: %.3f\n" targetFDR
                        reportResults results

    case result of
        Left (e :: SomeException) -> printf "\nExecution failed with exception: %s\n" (show e)
        Right _ -> printf "\n=== Case Study Complete ===\n"


-- Define the research program (the sequence of tests)
runCaseStudy :: CaseStudyContext -> FilePath -> [Hypothesis] -> Research LordState IO [TestResult]
runCaseStudy context workspaceBase hypotheses = do
    -- Initialize the scaffold runner functions, capturing the context.
    let (genScaffold, runLLM, execHarness) = initializeScaffoldRunner context

    results <- forM hypotheses $ \h -> do
        -- Call the generalized testHypothesis, passing the specialized implementations.
        testResultMaybe <- testHypothesis workspaceBase h True genScaffold runLLM execHarness

        -- Handle the result from testHypothesis (which now returns Maybe TestResult)
        let result = case testResultMaybe of
                Nothing -> TestResult (hId h) (hName h) Nothing Nothing False
                Just tr -> tr
        
        -- Debug logging using the correct logMessage from ResearchMonad
        logMessage $ printf "Main: H%d result - Discovery: %s, P-Value: %s" 
            (hId h) 
            (show $ trWasDiscovery result)
            (maybe "None" (printf "%.5f") (trPValue result))
            
        return result
    return results

-- Report the results in a clean table format
reportResults :: [TestResult] -> IO ()
reportResults results = do
    printf "%-5s | %-30s | %-9s | %-9s | %s\n" "ID" "Name" "P-Value" "Alpha_t" "Discovery"
    printf "%.0s\n" (replicate 70 '-')
    forM_ results $ \r -> do
        let pValStr = maybe "N/A" (printf "%.5f") (trPValue r)
        let alphaTStr = maybe "N/A" (printf "%.5f") (trAlphaT r)
        let discoveryStr = if trWasDiscovery r then "True" else "False"
        printf "%-5d | %-30s | %-9s | %-9s | %s\n"
            (trHypothesisId r) (T.unpack $ T.take 30 $ trName r) pValStr alphaTStr discoveryStr
    let discoveries = filter trWasDiscovery results
    printf "\nTotal Discoveries: %d / %d\n" (length discoveries) (length results)