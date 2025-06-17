module RVRS.Typecheck.Types where

import Data.Map

import RVRS.AST (type Typed)

type TypeEnv = Map String Typed

data TypeError
  = TypeMismatch Typed Typed
  | NotExpected Typed Typed
  | UnknownVariable String
  | UnsupportedOp String
  deriving (Show, Eq)
