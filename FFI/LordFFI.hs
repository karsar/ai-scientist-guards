{-# LANGUAGE ForeignFunctionInterface #-}

-- LordFFI: a Haskell harness that drives the GNATprove-verified LORD++
-- wealth arithmetic directly, via the C ABI that the SPARK kernel exports.
-- It demonstrates that the budget-critical multiplicative update can be
-- evaluated by the proved code (Lord_Capi) rather than by a separate,
-- unverified Haskell copy.

module Main where

import Text.Printf (printf)

foreign import ccall unsafe "lord_new_wealth"
  c_new_wealth :: Double -> Double -> Double   -- (1 - gamma) * W,  proved >= 0

foreign import ccall unsafe "lord_alpha"
  c_alpha :: Double -> Double -> Double          -- gamma * W,  proved >= 0

-- Thread the verified update over a gamma sequence: (alpha_t, wealth_{t+1}).
runLord :: Double -> [Double] -> [(Double, Double)]
runLord _  []       = []
runLord w (g : gs)  = (c_alpha w g, w') : runLord w' gs
  where w' = c_new_wealth w g

main :: IO ()
main = do
  let w0     = 0.025
      gammas = [0.0535, 0.0290, 0.0190, 0.0140, 0.0110]
      trace  = runLord w0 gammas
  putStrLn "Haskell orchestrator -> verified SPARK kernel (FFI):"
  putStrLn "  t   gamma     alpha_t      wealth"
  mapM_ (\(t, (g, (a, w))) ->
            printf "  %d   %.4f    %.6f    %.6f\n" (t :: Int) g a w)
        (zip [1 ..] (zip gammas trace))
  let minW = minimum (w0 : map snd trace)
  printf "min wealth observed = %.6f  (kernel postcondition: >= 0)\n" minW
