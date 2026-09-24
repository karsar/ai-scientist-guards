{-# LANGUAGE ForeignFunctionInterface #-}

-- LordEq1FFI: a Haskell harness that drives the GNATprove-verified Eq. 1
-- kernel (Lord_Eq1, through its C interface Lord_Eq1_Capi) and checks it
-- against calculateNextAlpha of Lord.hs (through advanceState).
--
-- Both run on the same p-value sequences: the paper's Tables 1, 4 and 5,
-- and 100 Monte Carlo runs (N = 2000, W0 = 0.1 * alpha) with the same
-- fixed seeds as Monte_Carlo_validation/tools/Eq1Reference.hs. The kernel
-- gets the gamma table of Lord.hs, so the two must agree bit for bit.
--
-- The paper's experiments still use Lord.hs. This harness only checks that
-- the verified kernel computes the same thresholds.

module Main where

import Control.Monad (forM, forM_, replicateM, unless, when)
import qualified Data.Vector.Unboxed as UV
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import GHC.Float (castDoubleToWord64)
import System.Exit (exitFailure)
import System.Random.MWC (initialize, uniform, GenIO)
import System.Random.MWC.Distributions (beta)
import Text.Printf (printf)

import Lord (LordConfig (..), LordState (..))
import Protocol (ProtocolError, StatisticalProtocol (..))

foreign import ccall unsafe "lord_eq1_init"
  c_init :: CDouble -> CDouble -> Ptr CInt -> IO ()

foreign import ccall unsafe "lord_eq1_set_gamma"
  c_set_gamma :: CInt -> CDouble -> Ptr CInt -> IO ()

foreign import ccall unsafe "lord_eq1_advance"
  c_advance :: CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe "lord_eq1_clamp_count"
  c_clamp_count :: Ptr CInt -> IO ()

-- | Call a C procedure whose last argument is an int status; fail if not 0.
checked :: String -> (Ptr CInt -> IO ()) -> IO ()
checked name f = alloca $ \st -> do
  f st
  s <- peek st
  unless (s == 0) $ do
    printf "%s returned status %d\n" name (fromIntegral s :: Int)
    exitFailure

-- | One step of the verified kernel: (alpha_t, reject).
kernelStep :: Double -> IO (Double, Bool)
kernelStep p =
  alloca $ \pa -> alloca $ \pr -> alloca $ \st -> do
    c_advance (CDouble p) pa pr st
    s <- peek st
    unless (s == 0) $ do
      printf "lord_eq1_advance returned status %d\n" (fromIntegral s :: Int)
      exitFailure
    CDouble a <- peek pa
    r <- peek pr
    return (a, r == 1)

clampCount :: IO Int
clampCount = alloca $ \pc -> c_clamp_count pc >> fromIntegral <$> peek pc

-- | Result of one sequence: (steps, bit-identical alphas, decision
--   differences, clamp count).
runBoth :: LordConfig -> [Double] -> IO (Int, Int, Int, Int)
runBoth cfg ps = do
  s0 <- either (error . show) return (initializeState cfg)
  checked "lord_eq1_init" (c_init (CDouble (alphaOverall cfg)) (CDouble (w0 cfg)))
  let go _ [] acc = return acc
      go s (p : rest) (n, same, diff) = do
        let (rejHs, aHs, s') = advanceState p s :: (Bool, Double, LordState)
        (aK, rejK) <- kernelStep p
        let same' = if castDoubleToWord64 aK == castDoubleToWord64 aHs then same + 1 else same
            diff' = if rejK /= rejHs then diff + 1 else diff
        go s' rest (n + 1, same', diff')
  (n, same, diff) <- go s0 ps (0, 0, 0)
  c <- clampCount
  return (n, same, diff, c)

mcPValues :: GenIO -> Int -> IO [Double]
mcPValues gen n = replicateM n $ do
  u <- uniform gen :: IO Double
  if u < 0.1 then beta 0.15 1.0 gen else uniform gen

main :: IO ()
main = do
  let alpha = 0.05
      cfgT1 = LordConfig { alphaOverall = alpha, w0 = 0.1 * alpha, maxHypotheses = 2000 }
      cfgHalf = cfgT1 { w0 = alpha / 2 }
      tables =
        [ ("Table 1",          cfgT1,   [0.00009, 0.04784, 0.00001, 0.32467, 0.13846])
        , ("Table 4 (perm.)",  cfgHalf, [0.856, 0.250, 1.000, 0.773, 0.125])
        , ("Table 4 (CV)",     cfgHalf, [1.000, 0.00004, 0.00006, 0.016, 0.00009])
        , ("Table 5",          cfgHalf, [5.0e-5, 8.0e-4, 1.0, 5.0e-5, 1.0])
        ]

  -- The gamma table of Lord.hs, loaded into the kernel
  s <- either (error . show) return (initializeState cfgT1 :: Either ProtocolError LordState)
  let g = gammaSeq s
  forM_ [1 .. UV.length g - 1] $ \j ->
    checked "lord_eq1_set_gamma" (c_set_gamma (fromIntegral j) (CDouble (g UV.! j)))

  putStrLn "Haskell (Lord.hs) vs verified SPARK Eq. 1 kernel (FFI):"
  tableResults <- forM tables $ \(name, cfg, ps) -> do
    r@(n, same, diff, c) <- runBoth cfg ps
    printf "  %-16s steps %4d  bit-identical %4d  decisions differ %d  clamps %d\n"
      (name :: String) n same diff c
    return r
  mcResults <- forM [1 .. 100 :: Int] $ \run -> do
    gen <- initialize (UV.fromList [fromIntegral run, 20260924])
    ps <- mcPValues gen 2000
    runBoth cfgT1 ps
  let total rs = foldr (\(a, b, c, d) (a', b', c', d') -> (a + a', b + b', c + c', d + d')) (0, 0, 0, 0) rs
      (n, same, diff, c) = total mcResults
  printf "  %-16s steps %d  bit-identical %d  decisions differ %d  clamps %d\n"
    ("Monte Carlo" :: String) n same diff c
  let (n', same', diff', c') = total (tableResults ++ mcResults)
  when (same' /= n' || diff' /= 0 || c' /= 0) $ do
    putStrLn "MISMATCH between Lord.hs and the verified kernel"
    exitFailure
  putStrLn "All thresholds bit-identical, no decision differs, Clamp_Count = 0."
