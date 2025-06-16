module RVRS.Env
  ( TypedVal(..)
  , ValueEnv
  , TypeEnv
  ) where

import qualified Data.Map as Map
import RVRS.AST (Value, Typed)

data TypedVal = TypedVal Typed Value
  deriving (Show, Eq)

type ValueEnv = Map.Map String Value
type TypeEnv  = Map.Map String TypedVal
