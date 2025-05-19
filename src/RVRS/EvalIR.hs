{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module RVRS.EvalIR (evalIRFlow, evalIRExpr, evalIRStmt, runEvalIR, EvalIR, EvalError(..)) where

import RVRS.IR
import RVRS.Value (Value(..))
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)

-- stdlib loading
import RVRS.Parser (parseRVRS)
import RVRS.Lower (lowerFlow)
import qualified RVRS.AST as AST
import System.IO (readFile)

-- Environments
type Env = Map.Map String Value
type FlowEnv = Map.Map String FlowIR

-- Evaluation monad
type EvalIR a = ReaderT FlowEnv (StateT Env (ExceptT EvalError IO)) a

-- Run evaluation
runEvalIR :: FlowEnv -> Env -> EvalIR a -> IO (Either EvalError (a, Env))
runEvalIR flows env action =
  runExceptT (runStateT (runReaderT action flows) env)

-- Evaluation errors
data EvalError
  = RuntimeError String
  | Return Value
  deriving (Show, Eq)

-- Scoped execution
isolate :: EvalIR a -> EvalIR a
isolate action = do
  env <- get
  flowMap <- ask
  let inner = runReaderT action flowMap
  result <- lift $ lift $ runStateT inner env
  return (fst result)

-- Load stdlib
loadAndMergeStdlib :: IO FlowEnv
loadAndMergeStdlib = do
  src <- readFile "stdlib/stdlib.rvrs"
  case parseRVRS src of
    Left err -> error $ "Stdlib parse error:\n" ++ show err
    Right flows ->
      return $ Map.fromList [(AST.flowName f, lowerFlow f) | f <- flows]


-- Flow entry
evalIRFlow :: FlowEnv -> String -> [Value] -> IO (Either EvalError (Maybe Value))
evalIRFlow userFlows entryName args = do
  stdlibFlows <- loadAndMergeStdlib
  let fullFlowMap = Map.union userFlows stdlibFlows
  case Map.lookup entryName fullFlowMap of
    Just (FlowIR _ params body) -> do
      let initialEnv = Map.fromList (zip params args)
      result <- runEvalIR fullFlowMap initialEnv $
                  catchError
                    (evalStmtsWithEnv body)
                    handleReturn
      return $ fmap fst result
    Nothing ->
      return $ Left (RuntimeError $ "No flow named '" ++ entryName ++ "' found.")
  where
    handleReturn :: EvalError -> EvalIR (Maybe Value)
    handleReturn (Return v) = return (Just v)
    handleReturn err        = throwError err

-- Evaluate list of statements
evalStmtsWithEnv :: [StmtIR] -> EvalIR (Maybe Value)
evalStmtsWithEnv [] = return Nothing
evalStmtsWithEnv (stmt:rest) = do
  result <- evalIRStmt stmt
  case result of
    Just val -> return (Just val)
    Nothing  -> evalStmtsWithEnv rest

-- Evaluate single statement
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
    throwError (Return val)

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

  IRDelta name expr -> do
    val <- evalIRExpr expr
    modify (Map.insert name val)
    return Nothing

  IRSource name expr -> do
    val <- evalIRExpr expr
    env <- get
    case Map.lookup name env of
      Nothing -> modify (Map.insert name val) >> return Nothing
      Just _  -> throwError $ RuntimeError ("Variable '" ++ name ++ "' already defined")

-- Evaluate expressions
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
    flowMap <- ask
    argVals <- mapM evalIRExpr args
    case Map.lookup name flowMap of
      Just (FlowIR _ params body) -> do
        let callEnv = Map.fromList (zip params argVals)
        (result, _) <- lift $ lift $ runStateT (runReaderT (evalStmtsWithEnv body) flowMap) callEnv
        case result of
          Just v  -> return v
          Nothing -> return VVoid
      Nothing -> throwError $ RuntimeError ("Unknown function: " ++ name)

-- Helpers
binOp :: (Double -> Double -> Double) -> ExprIR -> ExprIR -> EvalIR Value
binOp op a b = do
  v1 <- evalIRExpr a
  v2 <- evalIRExpr b
  case (v1, v2) of
    (VNum n1, VNum n2) -> return $ VNum (op n1 n2)
    _ -> throwError $ RuntimeError "Type error in arithmetic operation"
