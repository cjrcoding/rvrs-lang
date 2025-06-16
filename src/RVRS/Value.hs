-- src/RVRS/Value.hs

module RVRS.Value (Binding(..), valueToType, formatVal) where

import Ya (is, ho, hu, la, li)

import RVRS.AST (type Value)
import RVRS.Parser.Type (RVRSType(..))

-- | Variable bindings (mutable or immutable)
data Binding
  = Mutable Value
  | Immutable Value
  deriving (Show, Eq)

-- | Convert a runtime Value to its corresponding RVRSType
valueToType :: Value -> RVRSType
valueToType = is @String `hu` TypeStr `la` is @Double `hu` TypeNum `la` is @Bool `hu` TypeBool

-- | Format Value into a human-readable string
formatVal :: Value -> String
formatVal = is @String `la` is @Double `ho` show `la` is @Bool `ho` show
