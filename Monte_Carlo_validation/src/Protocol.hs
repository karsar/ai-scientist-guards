-- | Protocol.hs
-- Defines the generalized interface for sequential statistical protocols.
{-# LANGUAGE TypeFamilies #-}
module Protocol (
    StatisticalProtocol(..),
    ProtocolError(..)
) where

data ProtocolError = InvalidTransitionError String
                   | InitializationError String
                   deriving (Show, Eq)

-- | A Type Class defining the interface for a sequential statistical protocol.
--   's' represents the state maintained by the protocol.
class (Show s, Eq s) => StatisticalProtocol s where
    -- | The type for configuration parameters specific to the protocol.
    type Config s
    
    -- | Initializes the state of the protocol. Returns Right state or Left error.
    initializeState :: Config s -> Either ProtocolError s
    
    -- | Advances the state. 
    --   Takes the P-Value and the current state.
    --   Returns (IsDiscovery, Alpha_t, NewState). This must be a pure function.
    advanceState :: Double -> s -> (Bool, Double, s)

    -- | Helper function to check if a state transition is valid.
    --   Implementation is mandatory (no default).
    isValidTransition :: s -> s -> Either ProtocolError ()
