module RVRS.Typecheck.Types where

import qualified Data.Map as Map

data RVRS_Type
  = TNum
  | TStr
  | TBool
  | TUnknown
  deriving (Show, Eq)

type TypeEnv = Map.Map String RVRS_Type

data TypeError
  = TypeMismatch RVRS_Type RVRS_Type
  | UnknownVariable String
  | UnsupportedOp String
  deriving (Show, Eq)
