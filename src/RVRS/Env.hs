module RVRS.Env
  ( TypedVal(..)
  , ValueEnv
  , TypeEnv
  ) where

import qualified Data.Map as Map
import RVRS.AST (Value)

-- TEMPORARY: define here until TypeCheck exists
data RVRSValueType = VNum | VStr | VBool
  deriving (Show, Eq)

data TypedVal = TypedVal RVRSValueType Value
  deriving (Show, Eq)

type ValueEnv = Map.Map String Value
type TypeEnv  = Map.Map String TypedVal
