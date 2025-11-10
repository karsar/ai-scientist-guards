-- src/Protocol.hs
{-# LANGUAGE TypeFamilies #-}
module Protocol (
    StatisticalProtocol(..),
    ProtocolError(..)
) where

data ProtocolError = InvalidTransitionError String
                   | InitializationError String
                   deriving (Show, Eq)

class (Show s, Eq s) => StatisticalProtocol s where
    type Config s
    -- Initialization returns Either for robust error handling
    initializeState :: Config s -> Either ProtocolError s
    -- Pure state advancement
    advanceState :: Double -> s -> (Bool, Double, s)
    -- Mandatory validation of state transitions
    isValidTransition :: s -> s -> Either ProtocolError ()