-- src/RVRS/Value.hs

module RVRS.Value (Binding(..), valueToType, formatVal) where

import Ya (is, ho, hu, la, li, pattern Unit)

import RVRS.AST (type Value, type Typed, pattern String, pattern Double, pattern Bool)

-- | Variable bindings (mutable or immutable)
data Binding
  = Mutable Value
  | Immutable Value
  deriving (Show, Eq)

-- | Convert a runtime Value to its corresponding RVRSType
valueToType :: Value -> Typed
valueToType = is @String `hu` String Unit `la` is @Double `hu` Double Unit `la` is @Bool `hu` Bool Unit

-- | Format Value into a human-readable string
formatVal :: Value -> String
formatVal = is @String `la` is @Double `ho` show `la` is @Bool `ho` show
