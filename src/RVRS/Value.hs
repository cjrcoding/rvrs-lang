module RVRS.Value (Binding(..), valueToType, formatVal) where

import Prelude
import Data.Bool (bool)

import Ya (is, ho, ho'he, hu, hu'he, he'hu, la, li, pattern Unit, unwrap)

import RVRS.AST (type Value, type Typed, pattern String, pattern Double, pattern Bool)

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
formatVal = unwrap `la` is `ho'he` show @Double `la` is `ho'he` bool "false" "true"
