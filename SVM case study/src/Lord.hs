-- src/Lord.hs
{-# LANGUAGE TypeFamilies #-}
module Lord (
    LordState(..),
    LordConfig(..)
) where

import qualified Data.Vector.Unboxed as UV
import Control.Monad.State
import Protocol (StatisticalProtocol(..), ProtocolError(..))
import ResearchMonad (Discovery(..))
import Workspace (Hypothesis)
import Text.Printf (printf)

-- Configuration
data LordConfig = LordConfig {
    alphaOverall :: Double,
    w0 :: Double,
    maxHypotheses :: Int
} deriving (Show, Eq)

-- State (Internal fields are not exported directly)
data LordState = LordState {
    config :: LordConfig,
    gammaSeq :: UV.Vector Double,
    discoveries :: UV.Vector Int,
    currentTime :: Int,
    lsTestCount :: Int,
    lsDiscoveries :: [Discovery],
    lsRemainingAlpha :: Double
} deriving (Show, Eq)

-- Instance Implementation
instance StatisticalProtocol LordState where
    type Config LordState = LordConfig
    
    initializeState cfg =
        if w0 cfg > alphaOverall cfg || w0 cfg < 0
        then Left $ InitializationError "W0 must be between 0 and alphaOverall."
        else Right $ LordState {
            config = cfg,
            gammaSeq = generateGammaSeq (maxHypotheses cfg + 10),
            discoveries = UV.empty,
            currentTime = 1,
            lsTestCount = 0,
            lsDiscoveries = [],
            lsRemainingAlpha = alphaOverall cfg
        }

    advanceState p_value state = (isDiscovery, alpha_t, newState)
      where
        t = currentTime state
        alpha_t = calculateNextAlpha state
        isDiscovery = p_value <= alpha_t
        newDiscoveries = if isDiscovery then UV.snoc (discoveries state) t else discoveries state
        newState = state { 
            currentTime = t + 1, 
            discoveries = newDiscoveries,
            lsTestCount = t,  -- Update test count
            lsRemainingAlpha = if isDiscovery 
                              then lsRemainingAlpha state - alpha_t 
                              else lsRemainingAlpha state
        }

    isValidTransition oldState newState = 
        if currentTime newState == currentTime oldState + 1
        then Right ()
        else Left $ InvalidTransitionError $ 
            printf "Time must advance sequentially. Old: %d, New: %d" (currentTime oldState) (currentTime newState)

-- Helper Functions (Internal)
generateGammaSeq :: Int -> UV.Vector Double
generateGammaSeq n = UV.generate (n + 1) generator
  where
    c = 0.07720838
    generator i
      | i <= 0 = 0.0
      | otherwise = let i' = fromIntegral i
                      in c * (log (max i' 2.0)) / (i' * exp (sqrt (log i')))

getGamma :: UV.Vector Double -> Int -> Double
getGamma gamma i = if i > 0 && i < UV.length gamma then gamma UV.! i else 0.0

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