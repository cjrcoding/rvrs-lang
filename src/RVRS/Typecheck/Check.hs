module RVRS.Typecheck.Check where

import RVRS.AST
import RVRS.Typecheck.Types
import qualified Data.Map as Map

-- Infer the type of an expression
typeOfExpr :: TypeEnv -> Expr a -> Either TypeError RVRS_Type
typeOfExpr _ (NumLit _) = Right TNum
typeOfExpr _ (StrLit _) = Right TStr
typeOfExpr _ (BoolLit _) = Right TBool
typeOfExpr env (Var name) =
  case Map.lookup name env of
    Just ty -> Right ty
    Nothing -> Left (UnboundVariable name)
typeOfExpr _ other = Left (UnsupportedExpr (show other))
