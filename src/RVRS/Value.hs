-- src/RVRS/Value.hs

module RVRS.Value (Value(..), Binding(..), valueToType, formatVal) where

import RVRS.Parser.Type (RVRSType(..))

-- | Values produced during RVRS evaluation
-- This is the unified Value type used across the interpreter
-- including expression evaluation and type enforcement.
data Value
  = VNum Double
  | VStr String
  | VBool Bool
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
valueToType (VNum _)   = TypeNum
valueToType (VStr _)   = TypeStr
valueToType (VBool _)  = TypeBool
valueToType VVoid      = TypeAny
valueToType (VError _) = TypeAny
valueToType VUnit      = TypeAny

-- | Format Value into a human-readable string
formatVal :: Value -> String
formatVal (VStr s)  = s
formatVal (VNum n)  = show n
formatVal (VBool b) = show b
formatVal VUnit     = "unit"
formatVal VVoid     = "void"
formatVal (VError e)= "error: " ++ e
