-- src/RVRS/Eval/Stmt.hs
module RVRS.Eval.EvalStmt (evalIRStmt, evalStmtsWithEnv) where

import RVRS.IR
import RVRS.Value (Value(..))
import RVRS.Eval.EvalExpr (evalIRExpr)
import RVRS.Eval.Types (EvalIR, EvalError(..))
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

-- Core block runner
evalStmtsWithEnv :: [StmtIR] -> EvalIR (Maybe Value)
evalStmtsWithEnv [] = return Nothing
evalStmtsWithEnv (stmt:rest) = do
  result <- evalIRStmt stmt
  case result of
    Just val -> return (Just val)
    Nothing  -> evalStmtsWithEnv rest

-- Isolate scope
isolate :: EvalIR a -> EvalIR a
isolate action = do
  env <- get
  flowMap <- ask
  let inner = runReaderT action flowMap
  result <- lift $ lift $ runStateT inner env
  return (fst result)

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
      Just (FlowIR _ params body) -> do
        argVals <- mapM evalIRExpr args
        let callEnv = Map.fromList (zip params argVals)
        (result, _) <- lift $ lift $ runStateT (runReaderT (evalStmtsWithEnv body) flowMap) callEnv
        return result
      Nothing -> throwError $ RuntimeError ("Unknown flow: " ++ name)

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
    case condVal of
      VBool True  -> isolate (evalStmtsWithEnv tBlock)
      VBool False -> isolate (evalStmtsWithEnv eBlock)
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


