-- app/Main.hs
module Main where

import Validator
import Simulator
import Lord
import Protocol () -- Ensure instances are visible

main :: IO ()
main = do
    putStrLn "=== AI-Scientist Statistical Validation ==="
    putStrLn "Starting large-scale simulation (N=2000, 100 runs)..."
    putStrLn "This may take a few minutes...\n"
    
    -- Monte Carlo Configuration
    let mcConfig = MonteCarloConfig {
            numRuns = 100,
            verbose = False
        }
    
    -- Simulation Configuration: Scaled up
    let n_hypotheses = 2000
    let simConfig = SimulationConfig {
            nHypotheses = n_hypotheses,
            propTrueEffects = 0.1,   -- 10% are true effects
            powerParam = 0.15        -- Moderate statistical power
        }
        
    -- LORD Configuration
    let targetFDR = 0.05
    let lordConfig = LordConfig {
            alphaOverall = targetFDR,
            w0 = 0.1 * targetFDR,
            maxHypotheses = n_hypotheses
        }
        
    runValidation mcConfig simConfig lordConfig
    
    putStrLn "\n=== Simulation Complete ==="
