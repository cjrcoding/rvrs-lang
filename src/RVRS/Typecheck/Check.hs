module RVRS.Typecheck.Check where

import Ya (Recursive (..))
import qualified Data.Map as Map

import RVRS.AST
import RVRS.Typecheck.Types

-- Infer the type of an expression
typeOfExpr :: TypeEnv -> Recursive Expression -> Either TypeError RVRS_Type
typeOfExpr _ (Recursive (NumLit _)) = Right TNum
typeOfExpr _ (Recursive (StrLit _)) = Right TStr
typeOfExpr _ (Recursive (BoolLit _)) = Right TBool
typeOfExpr env (Recursive (Var name)) =
  case Map.lookup name env of
    Just ty -> Right ty
    Nothing -> Left (UnknownVariable name)
typeOfExpr _ other = Left (UnsupportedOp (show other))
