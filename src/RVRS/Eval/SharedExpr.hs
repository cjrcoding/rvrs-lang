-- src/RVRS/Eval/SharedExpr.hs
module RVRS.Eval.SharedExpr (
  evalSimpleExpr,
  binOp
) where

import RVRS.IR
import RVRS.Value (Value(..))
import RVRS.Eval.Types (EvalIR, EvalError(..))

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

-- Handles only basic expressions (literals + variables)
evalSimpleExpr :: ExprIR -> EvalIR Value
evalSimpleExpr expr = case expr of
  IRNumLit n  -> return $ VNum n
  IRStrLit s  -> return $ VStr s
  IRBoolLit b -> return $ VBool b
  IRVar name  -> do
    env <- get
    case Map.lookup name env of
      Just v  -> return v
      Nothing -> throwError $ RuntimeError ("Unbound variable: " ++ name)
  _ -> throwError $ RuntimeError "Unsupported expression in evalSimpleExpr"

-- Shared binary op helper
binOp :: (Double -> Double -> Double) -> ExprIR -> ExprIR -> EvalIR Value
binOp op a b = do
  v1 <- evalSimpleExpr a
  v2 <- evalSimpleExpr b
  case (v1, v2) of
    (VNum n1, VNum n2) -> return $ VNum (op n1 n2)
    _ -> throwError $ RuntimeError "Type error in arithmetic operation"
