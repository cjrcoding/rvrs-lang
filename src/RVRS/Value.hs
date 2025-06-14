-- src/RVRS/Value.hs

module RVRS.Value (Value(..), Binding(..), valueToType, formatVal) where

import Ya (is, ho, hu, la, li)

import RVRS.AST (Primitive, pattern String, pattern Double, pattern Bool)
import RVRS.Parser.Type (RVRSType(..))

-- | Values produced during RVRS evaluation
-- This is the unified Value type used across the interpreter
-- including expression evaluation and type enforcement.
data Value
  = VPrim Primitive
  | VError String
  | VVoid
  | VUnit
  deriving (Eq, Show)

-- | Variable bindings (mutable or immutable)
data Binding
  = Mutable Value
  | Immutable Value
  deriving (Show, Eq)

-- | Convert a runtime Value to its corresponding RVRSType
valueToType :: Value -> RVRSType
valueToType (VPrim x) = is @String `hu` TypeStr `la` is @Double `hu` TypeNum `la` is @Bool `hu` TypeBool `li` x
valueToType VVoid      = TypeAny
valueToType (VError _) = TypeAny
valueToType VUnit      = TypeAny

-- | Format Value into a human-readable string
formatVal :: Value -> String
formatVal (VPrim x) = is @String `la` is @Double `ho` show `la` is @Bool `ho` show `li` x
formatVal VUnit     = "unit"
formatVal VVoid     = "void"
formatVal (VError e)= "error: " ++ e
