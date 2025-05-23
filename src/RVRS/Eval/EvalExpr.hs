-- src/RVRS/Eval/Expr.hs
module RVRS.Eval.EvalExpr (evalIRExpr) where

import RVRS.IR
import RVRS.Value (Value(..)) 
import RVRS.Eval.Types (EvalIR, EvalError(..))


import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State


-- Evaluate expressions

binOp :: (Double -> Double -> Double) -> ExprIR -> ExprIR -> EvalIR Value
binOp op a b = do
  v1 <- evalIRExpr a
  v2 <- evalIRExpr b
  case (v1, v2) of
    (VNum n1, VNum n2) -> return $ VNum (op n1 n2)
    _ -> throwError $ RuntimeError "Type error in arithmetic operation"

evalIRExpr :: ExprIR -> EvalIR Value
evalIRExpr expr = case expr of
  IRNumLit n     -> return $ VNum n
  IRStrLit s     -> return $ VStr s
  IRBoolLit b    -> return $ VBool b

  IRVar name -> do
    env <- get
    case Map.lookup name env of
      Just v  -> return v
      Nothing -> throwError $ RuntimeError ("Unbound variable: " ++ name)

  IRAdd a b -> binOp (+) a b
  IRSub a b -> binOp (-) a b
  IRMul a b -> binOp (*) a b

  IRDiv a b -> do
    v1 <- evalIRExpr a
    v2 <- evalIRExpr b
    case (v1, v2) of
      (VNum _, VNum 0)      -> throwError $ RuntimeError "Division by zero"
      (VNum n1, VNum n2)    -> return $ VNum (n1 / n2)
      _                     -> throwError $ RuntimeError "Type error in division"

  IRNeg e -> do
    v <- evalIRExpr e
    case v of
      VNum n -> return $ VNum (-n)
      _      -> throwError $ RuntimeError "Negation requires number"

  IRNot e -> do
    v <- evalIRExpr e
    case v of
      VBool b -> return $ VBool (not b)
      _       -> throwError $ RuntimeError "Expected boolean in 'not'"

  IREquals a b -> do
    v1 <- evalIRExpr a
    v2 <- evalIRExpr b
    return $ VBool (v1 == v2)

  IRGreaterThan a b -> do
    v1 <- evalIRExpr a
    v2 <- evalIRExpr b
    case (v1, v2) of
      (VNum n1, VNum n2) -> return $ VBool (n1 > n2)
      _ -> throwError $ RuntimeError "> requires numeric values"

  IRLessThan a b -> do
    v1 <- evalIRExpr a
    v2 <- evalIRExpr b
    case (v1, v2) of
      (VNum n1, VNum n2) -> return $ VBool (n1 < n2)
      _ -> throwError $ RuntimeError "< requires numeric values"

  IRAnd a b -> do
    v1 <- evalIRExpr a
    v2 <- evalIRExpr b
    case (v1, v2) of
      (VBool b1, VBool b2) -> return $ VBool (b1 && b2)
      _ -> throwError $ RuntimeError "and requires booleans"

  IROr a b -> do
    v1 <- evalIRExpr a
    v2 <- evalIRExpr b
    case (v1, v2) of
      (VBool b1, VBool b2) -> return $ VBool (b1 || b2)
      _ -> throwError $ RuntimeError "or requires booleans"