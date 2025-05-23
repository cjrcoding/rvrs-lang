module RVRS.Types
  ( RVRSValueType(..)
  , TypeError(..)
  , TypedVal(..)
  , TypeEnv
  ) where

import qualified Data.Map as Map
import RVRS.Value (Value)

-- Supported types in RVRS
data RVRSValueType = VNum | VStr | VBool
  deriving (Eq, Show)

-- Type error representation
data TypeError
  = TypeMismatch String
  | UnknownVariable String
  deriving (Eq, Show)

-- Type + Value pair for the type checker
data TypedVal = TypedVal RVRSValueType Value
  deriving (Show, Eq)

-- Type-checking environment (used by typeCheckExpr)
type TypeEnv = Map.Map String TypedVal
