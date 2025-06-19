module RVRS.Value (Binding(..), valueToType, formatVal) where

import Prelude hiding (Bool (..))

import Ya (is, ho, hu, la, li, pattern Unit, type Boolean, pattern False, pattern True)

import RVRS.AST (type Value, type Typed, pattern String, pattern Double, pattern Bool)

-- | Variable bindings (mutable or immutable)
data Binding
  = Mutable Value
  | Immutable Value
  deriving (Show, Eq)

-- | Convert a runtime Value to its corresponding RVRSType
valueToType :: Value -> Typed
valueToType = is @String `hu` String Unit `la` is @Double `hu` Double Unit `la` is @Boolean `hu` Bool Unit

-- | Format Value into a human-readable string
formatVal :: Value -> String
formatVal = is @String `la` is @Double `ho` show `la` is @Boolean `ho` (False `hu` "false" `la` True `hu` "true")
