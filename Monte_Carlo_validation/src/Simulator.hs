-- Simulator.hs
module Simulator (
    GroundTruth(..),
    SimulationConfig(..),
    generateHypotheses
) where

import ResearchMonad
import System.Random.MWC (GenIO, createSystemRandom, uniform)
import System.Random.MWC.Distributions (beta)
import Control.Monad (replicateM)
import System.FilePath ((</>))
import Text.Printf (printf)

data GroundTruth = NullEffect | TrueEffect deriving (Show, Eq)

data SimulationConfig = SimulationConfig {
    nHypotheses :: Int,
    propTrueEffects :: Double,
    powerParam :: Double
} deriving (Show)

-- | Generate a stream of Hypotheses with known ground truth.
generateHypotheses :: SimulationConfig -> IO [(Int, (Hypothesis, GroundTruth))]
generateHypotheses cfg = do
    gen <- createSystemRandom
    results <- replicateM (nHypotheses cfg) (generateSingleHypothesis cfg gen)
    return $ zip [1..] results

-- | Generate a single hypothesis and its ground truth.
generateSingleHypothesis :: SimulationConfig -> GenIO -> IO (Hypothesis, GroundTruth)
generateSingleHypothesis cfg gen = do
    -- 1. Determine Ground Truth
    isTrueEffect <- (< propTrueEffects cfg) <$> uniform gen
    let truth = if isTrueEffect then TrueEffect else NullEffect
    
    -- 2. Define the DataContract and TestSpec
    let contract = DataContract "/data/sim_explore.csv" "/data/sim_validate.csv"
    let testSpec = SimulationOnly
    
    -- 3. Define the Scaffold Generation function
    let scaffoldGenerator = createSimulationScaffoldGenerator cfg gen truth
    
    -- 4. Create the Hypothesis data structure
    let hypothesis = Hypothesis {
            hId = 0,
            description = "Simulated (" ++ show truth ++ ")",
            hContract = contract,
            hTestSpec = testSpec,
            generateScaffold = scaffoldGenerator
        }
    return (hypothesis, truth)

-- | Creates the function that generates the Scaffold for the simulation.
--   Now returns Either ResearchError Scaffold.
createSimulationScaffoldGenerator :: SimulationConfig -> GenIO -> GroundTruth -> (FilePath -> Hypothesis -> IO (Either ResearchError Scaffold))
createSimulationScaffoldGenerator cfg gen truth = \workspaceDir hypothesis -> do
    let hIdVal = hId hypothesis
    let scaffoldDir = workspaceDir </> printf "H%04d" hIdVal
    
    -- The execution harness directly calls the P-value simulation logic.
    let harnessAction = do
            p_val <- simulatePValue cfg gen truth
            return (Just p_val)
            
    let scaffold = Scaffold {
        scaffoldPath = scaffoldDir,
        contract = hContract hypothesis,
        testSpec = hTestSpec hypothesis,
        executeHarness = harnessAction
    }
    -- In the simulation, generation always succeeds.
    return $ Right scaffold

-- | Simulate the p-value based on the ground truth.
simulatePValue :: SimulationConfig -> GenIO -> GroundTruth -> IO Double
simulatePValue cfg gen truth = case truth of
    NullEffect -> uniform gen
    TrueEffect -> beta (powerParam cfg) 1.0 gen
