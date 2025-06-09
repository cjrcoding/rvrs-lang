module RVRS.Typecheck.Check where

import Ya (Recursive (..), unwrap)
import qualified Data.Map as Map

import RVRS.AST
import RVRS.Typecheck.Types

-- Infer the type of an expression
typeOfExpr :: TypeEnv -> Recursive Expression -> Either TypeError RVRS_Type
typeOfExpr env expr = case unwrap expr of
  NumLit _ -> Right TNum
  StrLit _ -> Right TStr
  BoolLit _ -> Right TBool
  Var name -> case Map.lookup name env of
    Just ty -> Right ty
    Nothing -> Left $ UnknownVariable name
  other -> Left . UnsupportedOp $ show other
