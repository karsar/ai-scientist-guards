-- | Lord.hs
-- Implementation of the LORD++ algorithm as an instance of StatisticalProtocol.
{-# LANGUAGE TypeFamilies #-}
module Lord (
    LordConfig(..),
    LordState(..)
) where

import qualified Data.Vector.Unboxed as UV
import Protocol (StatisticalProtocol(..), ProtocolError(..))
import Text.Printf (printf)

-- | Configuration for the LORD++ algorithm
data LordConfig = LordConfig {
    alphaOverall :: Double, -- ^ The target FDR level (e.g., 0.05)
    w0 :: Double,           -- ^ Initial wealth (must be <= alphaOverall).
    maxHypotheses :: Int    -- ^ Maximum number of hypotheses
} deriving (Show, Eq)

-- | The evolving state of the LORD++ algorithm
data LordState = LordState {
    config :: LordConfig,
    gammaSeq :: UV.Vector Double,
    discoveries :: UV.Vector Int,
    currentTime :: Int
} deriving (Show, Eq)

-- ============================================================================
-- Protocol Instance Implementation
-- ============================================================================

instance StatisticalProtocol LordState where
    type Config LordState = LordConfig
    
    initializeState cfg =
        if w0 cfg > alphaOverall cfg || w0 cfg < 0
        then Left $ InitializationError "W0 must be between 0 and alphaOverall."
        else Right $ LordState {
            config = cfg,
            gammaSeq = generateGammaSeq (maxHypotheses cfg + 10),
            discoveries = UV.empty,
            currentTime = 1
        }

    advanceState p_value state = (isDiscovery, alpha_t, newState)
      where
        t = currentTime state
        alpha_t = calculateNextAlpha state
        isDiscovery = p_value <= alpha_t
        newDiscoveries = if isDiscovery
                           then UV.snoc (discoveries state) t
                           else discoveries state
        newState = state {
            currentTime = t + 1,
            discoveries = newDiscoveries
        }

    -- Enforce strict sequential time advancement
    isValidTransition oldState newState = 
        if currentTime newState == currentTime oldState + 1
        then Right ()
        else Left $ InvalidTransitionError $ 
            printf "Time must advance sequentially. Old: %d, New: %d" 
                   (currentTime oldState) (currentTime newState)

-- ============================================================================
-- Pure Helper Functions
-- ============================================================================

-- | Generate the gamma sequence.
generateGammaSeq :: Int -> UV.Vector Double
generateGammaSeq n = UV.generate (n + 1) generator
  where
    c = 0.07720838
    generator i
      | i <= 0 = 0.0
      | otherwise = let i' = fromIntegral i
                      in c * (log (max i' 2.0)) / (i' * exp (sqrt (log i')))

-- | Helper to safely access the gamma sequence (1-based indexing).
getGamma :: UV.Vector Double -> Int -> Double
getGamma gamma i = if i > 0 && i < UV.length gamma then gamma UV.! i else 0.0

-- | Calculate alpha_t for the current time step t based on the state.
calculateNextAlpha :: LordState -> Double
calculateNextAlpha state = alpha_t
  where
    t = currentTime state
    cfg = config state
    w0_val = w0 cfg
    alpha_overall_val = alphaOverall cfg
    gamma = gammaSeq state
    
    term1 = w0_val * (getGamma gamma t)
    
    payout_diff = alpha_overall_val - w0_val
    sum_redistributed = UV.sum $ UV.map (\tau_k -> getGamma gamma (t - tau_k)) (discoveries state)
    
    term2 = payout_diff * sum_redistributed
    
    alpha_t = term1 + term2
