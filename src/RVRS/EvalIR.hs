{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module RVRS.EvalIR (evalIRFlow, evalIRExpr, evalIRStmt, runEvalIR, EvalIR, EvalError(..)) where

import RVRS.IR
import RVRS.Value (Value(..))
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

-- We define our environment as before.
type Env = Map.Map String Value
type FlowEnv = Map.Map String FlowIR

-- =============================================================================
-- Refactored Evaluator Monad
--
-- Instead of a newtype that only “reads” an environment, we now use 
-- StateT to hold the environment so that changes (like IRDelta) persist.
--
-- Our monad is defined as follows:
--
--     type EvalIR a = StateT Env (ExceptT EvalError IO) a
--
-- This lets you:
--   • call 'get' to obtain the current environment,
--   • call 'modify' to update the environment,
--   • throw errors with 'throwError', and
--   • perform IO with 'liftIO'.
-- =============================================================================

type EvalIR a = StateT Env (ExceptT EvalError IO) a

-- runEvalIR is provided to kick off an evaluation.
runEvalIR :: Env -> EvalIR a -> IO (Either EvalError (a, Env))
runEvalIR env action = runExceptT (runStateT action env)

-- =============================================================================
-- Evaluation Errors
-- =============================================================================

data EvalError
  = RuntimeError String
  | Return Value
  deriving (Show, Eq)

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- Get the current environment (using StateT’s 'get')
getEnv :: EvalIR Env
getEnv = get

-- isolate: run a sub-action in an isolated copy of the current environment.
-- This is used in branch statements so that changes inside the branch do not
-- affect the outer environment.
isolate :: EvalIR a -> EvalIR a
isolate action = do
  env <- get
  -- Run the action starting with a copy of env; ignore any changes.
  (result, _) <- lift $ runStateT action env
  return result

-- =============================================================================
-- Entry Point
-- =============================================================================

-- evalIRFlow: Given a map of flows, an entry name, and argument values,
-- look up the appropriate flow and evaluate its body.
evalIRFlow :: FlowEnv -> String -> [Value] -> IO (Either EvalError (Maybe Value))
evalIRFlow flowMap entryName args = do
  case Map.lookup entryName flowMap of
    Just (FlowIR _ params body) -> do
      let initialEnv = Map.fromList (zip params args)
      result <- runEvalIR initialEnv (evalStmtsWithEnv flowMap body)
      return $ fmap fst result
    Nothing ->
      return $ Left (RuntimeError $ "No flow named '" ++ entryName ++ "' found.")


-- =============================================================================
-- Evaluating Statements
-- =============================================================================

-- Evaluate a list of statements sequentially.
evalStmtsWithEnv :: FlowEnv -> [StmtIR] -> EvalIR (Maybe Value)
evalStmtsWithEnv _ [] = return Nothing
evalStmtsWithEnv flowMap (stmt:rest) = do
  result <- evalIRStmt flowMap stmt
  case result of
    Just val -> return (Just val)
    Nothing  -> evalStmtsWithEnv flowMap rest

-- Evaluate a single IR statement.
evalIRStmt :: FlowEnv -> StmtIR -> EvalIR (Maybe Value)
evalIRStmt flowMap stmt = case stmt of
  IREcho expr -> do
    val <- evalIRExpr expr
    liftIO $ putStrLn ("echo: " ++ show val)
    return Nothing

  IRWhisper label expr -> do
    val <- evalIRExpr expr
    liftIO $ putStrLn ("→ whisper: " ++ label ++ " = " ++ show val)
    return Nothing

  IRCallStmt name args -> case Map.lookup name flowMap of
    Just (FlowIR _ params body) -> do
      argVals <- mapM evalIRExpr args
      -- For function calls, create an isolated environment for the call.
      let callEnv = Map.fromList (zip params argVals)
      -- Run the body with that environment and discard changes afterwards.
      lift $ runStateT (evalStmtsWithEnv flowMap body) callEnv >>= \(result, _) -> return result
    Nothing -> throwError $ RuntimeError ("Unknown flow: " ++ name)

  IRReturn expr -> do
    val <- evalIRExpr expr
    return (Just val)

  IRMouth expr -> do
    val <- evalIRExpr expr
    throwError (Return val)

  IRAssert expr -> do
    val <- evalIRExpr expr
    case val of
      VBool True -> return Nothing
      VBool False -> throwError $ RuntimeError "Assertion failed"
      _ -> throwError $ RuntimeError "Assert expects boolean"

  -- Branch: Evaluate a condition, then execute either the true-block or the false-block.
  -- We use 'isolate' so that any changes within the branch do not leak out.
  IRBranch cond tBlock eBlock -> do
    condVal <- evalIRExpr cond
    case condVal of
      VBool True  -> isolate (evalStmtsWithEnv flowMap tBlock)
      VBool False -> isolate (evalStmtsWithEnv flowMap eBlock)
      _ -> throwError $ RuntimeError "Condition must be boolean"

  -- Delta: Update the environment by inserting a new variable binding.
  -- Now that our evaluator is stateful, we use 'modify' to update the environment
  -- in a way that persists across statements.
  IRDelta name expr -> do
    val <- evalIRExpr expr
    modify (Map.insert name val)
    return Nothing

-- =============================================================================
-- Evaluating Expressions
-- =============================================================================

-- Evaluate an expression to a value.
evalIRExpr :: ExprIR -> EvalIR Value
evalIRExpr expr = case expr of
  IRNumLit n -> return $ VNum n
  IRStrLit s -> return $ VStr s
  IRBoolLit b -> return $ VBool b

  IRVar name -> do
    env <- get
    case Map.lookup name env of
      Just v -> return v
      Nothing -> throwError $ RuntimeError ("Unbound variable: " ++ name)

  IRAdd e1 e2 -> binOp (+) e1 e2
  IRSub e1 e2 -> binOp (-) e1 e2
  IRMul e1 e2 -> binOp (*) e1 e2

  IRDiv e1 e2 -> do
    v1 <- evalIRExpr e1
    v2 <- evalIRExpr e2
    case (v1, v2) of
      (VNum _, VNum 0) -> throwError $ RuntimeError "Division by zero"
      (VNum n1, VNum n2) -> return $ VNum (n1 / n2)
      _ -> throwError $ RuntimeError "Type error in division"

  IREquals e1 e2 -> do
    v1 <- evalIRExpr e1
    v2 <- evalIRExpr e2
    return $ VBool (v1 == v2)

  IRNot e -> do
    v <- evalIRExpr e
    case v of
      VBool b -> return $ VBool (not b)
      _ -> throwError $ RuntimeError "Expected boolean in 'not'"

-- A helper for evaluating binary arithmetic operations.
binOp :: (Double -> Double -> Double) -> ExprIR -> ExprIR -> EvalIR Value
binOp op e1 e2 = do
  v1 <- evalIRExpr e1
  v2 <- evalIRExpr e2
  case (v1, v2) of
    (VNum n1, VNum n2) -> return $ VNum (op n1 n2)
    _ -> throwError $ RuntimeError "Type error in arithmetic operation"
