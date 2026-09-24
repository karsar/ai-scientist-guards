-- | Eq1Reference: reference thresholds from Lord.hs for the SPARK Eq. 1 check.
--
-- This program runs the Haskell LORD implementation (initializeState and
-- advanceState of Lord.hs) on fixed p-value sequences and writes one CSV
-- row per step. Floating-point values are written as their IEEE 754 bit
-- patterns (16 hex digits), so the Ada driver can compare them bit for bit.
--
-- Sequences:
--   * mc     : the Monte Carlo configuration of app/Main.hs (N = 2000,
--              10% true effects, Beta(0.15, 1) alternatives, alpha = 0.05,
--              W0 = 0.1 * alpha), 100 runs. The p-values use the same
--              generator and distributions as Simulator.hs, but with fixed
--              seeds (run number) so that the runs can be repeated.
--   * table1 : Table 1 of the paper (W0 = 0.1 * alpha).
--   * table4_perm, table4_cv, table5 : Tables 4 and 5 (W0 = alpha / 2).
--
-- Usage: lord-eq1-reference STEPS_CSV GAMMA_CSV
module Main (main) where

import qualified Data.Vector.Unboxed as UV
import Control.Monad (forM, replicateM)
import GHC.Float (castDoubleToWord64)
import System.Environment (getArgs)
import System.IO
import System.Random.MWC (initialize, uniform, GenIO)
import System.Random.MWC.Distributions (beta)
import Text.Printf (printf, hPrintf)

import Lord (LordConfig(..), LordState(..))
import Protocol (StatisticalProtocol(..))

bits :: Double -> String
bits = printf "%016x" . castDoubleToWord64

-- | Run Lord.hs over a p-value sequence. Returns (p, alpha_t, reject).
runLord :: LordConfig -> [Double] -> [(Double, Double, Bool)]
runLord cfg ps = case initializeState cfg of
    Left err -> error (show err)
    Right s0 -> go s0 ps
  where
    go _ [] = []
    go s (p : rest) =
        let (rej, a, s') = advanceState p s :: (Bool, Double, LordState)
        in (p, a, rej) : go s' rest

-- | P-values of one Monte Carlo run: truth first, then the p-value, as in
--   Simulator.hs (uniform under the null, Beta(0.15, 1) otherwise).
mcPValues :: GenIO -> Int -> IO [Double]
mcPValues gen n = replicateM n $ do
    u <- uniform gen :: IO Double
    if u < 0.1 then beta 0.15 1.0 gen else uniform gen

main :: IO ()
main = do
    [stepsPath, gammaPath] <- getArgs
    let alpha = 0.05
        cfgMC  = LordConfig { alphaOverall = alpha, w0 = 0.1 * alpha, maxHypotheses = 2000 }
        cfgT1  = cfgMC
        cfgHalf = LordConfig { alphaOverall = alpha, w0 = alpha / 2, maxHypotheses = 2000 }
        tables =
          [ ("table1",      cfgT1,   [0.00009, 0.04784, 0.00001, 0.32467, 0.13846])
          , ("table4_perm", cfgHalf, [0.856, 0.250, 1.000, 0.773, 0.125])
          , ("table4_cv",   cfgHalf, [1.000, 0.00004, 0.00006, 0.016, 0.00009])
          , ("table5",      cfgHalf, [5.0e-5, 8.0e-4, 1.0, 5.0e-5, 1.0])
          ]
    mc <- forM [1 .. 100 :: Int] $ \run -> do
        gen <- initialize (UV.fromList [fromIntegral run, 20260924])
        ps <- mcPValues gen 2000
        return ("mc", run, cfgMC, ps)
    let cases = [ (name, 1, cfg, ps) | (name, cfg, ps) <- tables ] ++ mc
    withFile stepsPath WriteMode $ \h -> do
        hPutStrLn h "case,run,t,alpha,w0,p,alpha_hs,reject_hs"
        mapM_ (\(name, run, cfg, ps) ->
                 mapM_ (\(t, (p, a, rej)) ->
                          hPrintf h "%s,%d,%d,%s,%s,%s,%s,%d\n" (name :: String) run (t :: Int)
                            (bits (alphaOverall cfg)) (bits (w0 cfg)) (bits p) (bits a)
                            (if rej then 1 else 0 :: Int))
                       (zip [1 ..] (runLord cfg ps)))
              cases
    -- The gamma table Lord.hs uses (index 1 .. 2000)
    case initializeState cfgMC of
        Left err -> error (show err)
        Right s -> withFile gammaPath WriteMode $ \h -> do
            hPutStrLn h "j,gamma_hs"
            mapM_ (\j -> hPrintf h "%d,%s\n" j (bits (gammaSeq s UV.! j))) [1 .. 2000 :: Int]
    putStrLn "done"
