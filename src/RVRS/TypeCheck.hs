module RVRS.TypeCheck
  ( RVRSValueType(..)
  , TypeError(..)
  , typeCheckExpr
  ) where

import RVRS.IR (IRExpr(..))
import RVRS.Types
  ( RVRSValueType(..)
  , TypeError(..)
  , TypedVal(..)
  , TypeEnv
  , FlowSig(..)
  , FlowSigEnv
  )

import qualified Data.Map as Map

-- Main type checker for IR expressions
typeCheckExpr :: IRExpr -> TypeEnv -> FlowSigEnv -> Either TypeError RVRSValueType

-- Literals
typeCheckExpr (IRNumLit _) _ _   = Right VNum
typeCheckExpr (IRStrLit _) _ _   = Right VStr
typeCheckExpr (IRBoolLit _) _ _  = Right VBool

-- Variable lookup
typeCheckExpr (IRVar name) env _ =
  case Map.lookup name env of
    Just (TypedVal t _) -> Right t
    Nothing             -> Left (UnknownVariable name)

-- Arithmetic
typeCheckExpr (IRAdd e1 e2) env fs = numericBinOp "Add" e1 e2 env fs
typeCheckExpr (IRSub e1 e2) env fs = numericBinOp "Sub" e1 e2 env fs
typeCheckExpr (IRMul e1 e2) env fs = numericBinOp "Mul" e1 e2 env fs
typeCheckExpr (IRDiv e1 e2) env fs = numericBinOp "Div" e1 e2 env fs

-- Boolean logic
typeCheckExpr (IRAnd e1 e2) env fs = boolBinOp "and" e1 e2 env fs
typeCheckExpr (IROr  e1 e2) env fs = boolBinOp "or" e1 e2 env fs
typeCheckExpr (IRNot e)     env fs = do
  t <- typeCheckExpr e env fs
  if t == VBool
    then Right VBool
    else Left (TypeMismatch "not requires a Bool")

-- Comparison
typeCheckExpr (IRGreaterThan e1 e2) env fs = compareNum ">" e1 e2 env fs
typeCheckExpr (IRLessThan    e1 e2) env fs = compareNum "<" e1 e2 env fs

-- Equality
typeCheckExpr (IREquals e1 e2) env fs = compareEq "==" e1 e2 env fs
typeCheckExpr (IRNotEquals e1 e2) env fs = compareEq "!=" e1 e2 env fs

-- Negation
typeCheckExpr (IRNegate e) env fs = do
  t <- typeCheckExpr e env fs
  if t == VNum
    then Right VNum
    else Left (TypeMismatch "Negation requires a Num")

-- Function calls
typeCheckExpr (IRCall fname args) env fsenv =
  case Map.lookup fname fsenv of
    Nothing -> Left (UnknownVariable ("flow: " ++ fname))
    Just (FlowSig expectedArgs retType) ->
      if length args /= length expectedArgs
        then Left (TypeMismatch ("Arity mismatch in call to " ++ fname))
        else do
          actualTypes <- mapM (\arg -> typeCheckExpr arg env fsenv) args
          if actualTypes == expectedArgs
            then Right retType
            else Left (TypeMismatch ("Argument type mismatch in call to " ++ fname))

-- Fallback
typeCheckExpr _ _ _ = Left (TypeMismatch "Unsupported expression form")

-- Helpers
numericBinOp :: String -> IRExpr -> IRExpr -> TypeEnv -> FlowSigEnv -> Either TypeError RVRSValueType
numericBinOp label e1 e2 env fs = do
  t1 <- typeCheckExpr e1 env fs
  t2 <- typeCheckExpr e2 env fs
  if t1 == VNum && t2 == VNum
    then Right VNum
    else Left (TypeMismatch (label ++ " requires Num operands"))

boolBinOp :: String -> IRExpr -> IRExpr -> TypeEnv -> FlowSigEnv -> Either TypeError RVRSValueType
boolBinOp label e1 e2 env fs = do
  t1 <- typeCheckExpr e1 env fs
  t2 <- typeCheckExpr e2 env fs
  if t1 == VBool && t2 == VBool
    then Right VBool
    else Left (TypeMismatch (label ++ " requires Bool operands"))

compareNum :: String -> IRExpr -> IRExpr -> TypeEnv -> FlowSigEnv -> Either TypeError RVRSValueType
compareNum label e1 e2 env fs = do
  t1 <- typeCheckExpr e1 env fs
  t2 <- typeCheckExpr e2 env fs
  if t1 == VNum && t2 == VNum
    then Right VBool
    else Left (TypeMismatch (label ++ " requires Num operands"))

compareEq :: String -> IRExpr -> IRExpr -> TypeEnv -> FlowSigEnv -> Either TypeError RVRSValueType
compareEq _ e1 e2 env fs = do
  t1 <- typeCheckExpr e1 env fs
  t2 <- typeCheckExpr e2 env fs
  if t1 == t2
    then Right VBool
    else Left (TypeMismatch "Equality comparison requires both sides to have the same type")
