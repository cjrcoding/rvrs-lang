module RVRS.TypeCheck
  ( RVRSValueType(..)
  , TypeError(..)
  , typeCheckExpr
  ) where

import RVRS.IR (IRExpr(..))
import RVRS.Env (TypeEnv(..), TypedVal(..))
import RVRS.Types (RVRSValueType(..), TypeError(..), TypedVal(..), TypeEnv)
import qualified Data.Map as Map


-- Main type checker for expressions
typeCheckExpr :: IRExpr -> TypeEnv -> Either TypeError RVRSValueType

typeCheckExpr (IRNumLit _) _ = Right VNum
typeCheckExpr (IRStrLit _) _ = Right VStr
typeCheckExpr (IRBoolLit _) _ = Right VBool

typeCheckExpr (IRVar name) env =
  case Map.lookup name env of
    Just (TypedVal t _) -> Right t
    Nothing -> Left (UnknownVariable name)

typeCheckExpr (IRAdd e1 e2) env = do
  t1 <- typeCheckExpr e1 env
  t2 <- typeCheckExpr e2 env
  if t1 == VNum && t2 == VNum
    then Right VNum
    else Left (TypeMismatch "Add requires Num + Num")

typeCheckExpr (IRSub e1 e2) env = do
  t1 <- typeCheckExpr e1 env
  t2 <- typeCheckExpr e2 env
  if t1 == VNum && t2 == VNum
    then Right VNum
    else Left (TypeMismatch "Sub requires Num - Num")

typeCheckExpr (IRMul e1 e2) env = do
  t1 <- typeCheckExpr e1 env
  t2 <- typeCheckExpr e2 env
  if t1 == VNum && t2 == VNum
    then Right VNum
    else Left (TypeMismatch "Mul requires Num * Num")

typeCheckExpr (IRDiv e1 e2) env = do
  t1 <- typeCheckExpr e1 env
  t2 <- typeCheckExpr e2 env
  if t1 == VNum && t2 == VNum
    then Right VNum
    else Left (TypeMismatch "Div requires Num / Num")

typeCheckExpr (IREquals e1 e2) env = do
  t1 <- typeCheckExpr e1 env
  t2 <- typeCheckExpr e2 env
  if t1 == t2
    then Right VBool
    else Left (TypeMismatch "Equals requires both sides to be the same type")

typeCheckExpr (IRNotEquals e1 e2) env = do
  t1 <- typeCheckExpr e1 env
  t2 <- typeCheckExpr e2 env
  if t1 == t2
    then Right VBool
    else Left (TypeMismatch "NotEquals requires both sides to be the same type")

typeCheckExpr (IRNegate e) env = do
  t <- typeCheckExpr e env
  if t == VNum
    then Right VNum
    else Left (TypeMismatch "Negate requires a Num")

typeCheckExpr _ _ = Left (TypeMismatch "Unsupported expression form")
