module RVRS.Value (Binding(..), valueToType, formatVal) where

import Prelude
import Data.Bool (bool)

import Ya (is, ho, hu, he'hu, la, li, pattern Unit, unwrap)

import RVRS.Syntax (type Value, type Typed, pattern String, pattern Double, pattern Bool)

-- | Variable bindings (mutable or immutable)
data Binding
  = Mutable Value
  | Immutable Value
  deriving (Show, Eq)

-- | Convert a runtime Value to its corresponding RVRSType
valueToType :: Value -> Typed
valueToType = is `hu` String Unit `la` is `hu` Double Unit `la` is `hu` Bool Unit

-- | Format Value into a human-readable string
formatVal :: Value -> String
formatVal = is @String `la` is `ho` show @Double `la` is `ho` bool "false" "true"
