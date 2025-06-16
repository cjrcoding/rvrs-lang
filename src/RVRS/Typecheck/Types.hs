module RVRS.Typecheck.Types where

import qualified Data.Map as Map

import RVRS.AST (type Typed)

type TypeEnv = Map.Map String Typed

data TypeError
  = TypeMismatch Typed Typed
  | UnknownVariable String
  | UnsupportedOp String
  deriving (Show, Eq)
