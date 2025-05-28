module RVRS.Eval.EvalExpr (evalIRExpr) where

import RVRS.IR (ExprIR(..), StmtIR(..), FlowIR(..))
import RVRS.Value (Value(..)) 
import RVRS.Eval.Types (EvalIR, EvalError(..), FlowEnv)
import RVRS.Env (ValueEnv)

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

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

  IRCallExpr name args -> do
    fsenv <- ask
    case Map.lookup name fsenv of
      Nothing -> throwError $ RuntimeError ("Unknown function: " ++ name)
      Just (FlowIR _ paramNames body) -> do
        argVals <- mapM evalIRExpr args
        if length paramNames /= length argVals
          then throwError $ RuntimeError ("Arity mismatch calling: " ++ name)
          else do
            let callEnv = Map.fromList (zip paramNames argVals)
            (result, _) <- lift . lift $ runStateT (runReaderT (evalBody body) fsenv) callEnv
            return $ fromMaybe VVoid result

-- Statement evaluator

evalIRStmt :: StmtIR -> EvalIR (Maybe Value)
evalIRStmt stmt = case stmt of
  IREcho expr -> do
    val <- evalIRExpr expr
    liftIO $ putStrLn ("echo: " ++ show val)
    return Nothing

  IRWhisper label expr -> do
    val <- evalIRExpr expr
    liftIO $ putStrLn ("→ whisper: " ++ label ++ " = " ++ show val)
    return Nothing

  IRCallStmt name args -> do
    flowMap <- ask
    case Map.lookup name flowMap of
      Nothing -> throwError $ RuntimeError ("Unknown flow: " ++ name)
      Just (FlowIR _ params body) -> do
        argVals <- mapM evalIRExpr args
        let callEnv = Map.fromList (zip params argVals)
        _ <- lift . lift $ runStateT (runReaderT (evalBody body) flowMap) callEnv
        return Nothing

  IRReturn expr -> do
    val <- evalIRExpr expr
    return (Just val)

  IRMouth expr -> do
    val <- evalIRExpr expr
    liftIO $ putStrLn ("mouth: " ++ show val)
    return Nothing

  IRAssert expr -> do
    val <- evalIRExpr expr
    case val of
      VBool True  -> return Nothing
      VBool False -> throwError $ RuntimeError "Assertion failed"
      _           -> throwError $ RuntimeError "Assert expects boolean"

  IRBranch cond tBlock eBlock -> do
    condVal <- evalIRExpr cond
    let evalBlock [] = return Nothing
        evalBlock (s:ss) = do
          res <- evalIRStmt s
          case res of
            Just v  -> return (Just v)
            Nothing -> evalBlock ss
    case condVal of
      VBool True  -> evalBlock tBlock
      VBool False -> evalBlock eBlock
      _ -> throwError $ RuntimeError "Condition must be boolean"

  IRDelta name expr _mType -> do
    val <- evalIRExpr expr
    modify (Map.insert name val)
    return Nothing

  IRSource name expr _mType -> do
    val <- evalIRExpr expr
    env <- get
    case Map.lookup name env of
      Nothing -> modify (Map.insert name val) >> return Nothing
      Just _  -> throwError $ RuntimeError ("Variable '" ++ name ++ "' already defined")

-- Flow body evaluator used in both CallExpr and CallStmt

evalBody :: [StmtIR] -> EvalIR (Maybe Value)
evalBody [] = return Nothing
evalBody (stmt:rest) = do
  result <- evalIRStmt stmt
  case result of
    Just val -> return (Just val)
    Nothing  -> evalBody rest
