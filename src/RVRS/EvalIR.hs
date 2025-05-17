module RVRS.EvalIR (evalIRFlow, evalIRExpr, evalIRStmt) where

import RVRS.IR
import qualified Data.Map as Map
import RVRS.Value (Value(..))

type Env = Map.Map String Value
type FlowEnv = Map.Map String FlowIR

-- Entry point: run a named flow with args from the IR FlowEnv
evalIRFlow :: FlowEnv -> String -> [Value] -> IO (Maybe Value)
evalIRFlow flowMap entryName args = case Map.lookup entryName flowMap of
  Just (FlowIR _ params body) -> do
    let initialEnv = Map.fromList (zip params args)
    evalStmtsWithEnv flowMap body initialEnv
  Nothing -> do
    putStrLn $ "Error: No flow named '" ++ entryName ++ "' found."
    return Nothing

-- Evaluate a sequence of IR statements with full context
evalStmtsWithEnv :: FlowEnv -> [StmtIR] -> Env -> IO (Maybe Value)
evalStmtsWithEnv _ [] _ = return Nothing
evalStmtsWithEnv flowMap (stmt:rest) env = case stmt of
  IRDelta name expr -> do
    val <- evalIRExpr expr env
    let newEnv = Map.insert name val env
    evalStmtsWithEnv flowMap rest newEnv

  _ -> do
    result <- evalIRStmt flowMap stmt env
    case result of
      Just val -> return (Just val)
      Nothing  -> evalStmtsWithEnv flowMap rest env

-- Evaluate a single IR statement
evalIRStmt :: FlowEnv -> StmtIR -> Env -> IO (Maybe Value)
evalIRStmt flowMap stmt env = case stmt of
  IREcho expr -> do
    val <- evalIRExpr expr env
    putStrLn ("echo: " ++ show val)
    return Nothing

  IRWhisper label expr -> do
    val <- evalIRExpr expr env
    putStrLn ("→ whisper: " ++ label ++ " = " ++ show val)
    return Nothing


  IRCallStmt name args -> case Map.lookup name flowMap of
    Just (FlowIR _ params body) -> do
      argVals <- mapM (`evalIRExpr` env) args
      let callEnv = Map.fromList (zip params argVals)
      evalStmtsWithEnv flowMap body callEnv
    Nothing -> do
      putStrLn ("Unknown flow: " ++ name)
      return Nothing

  IRReturn expr -> do
    val <- evalIRExpr expr env
    return (Just val)

  IRBranch cond tBlock eBlock -> do
    condVal <- evalIRExpr cond env
    case condVal of
      VBool True  -> evalStmtsWithEnv flowMap tBlock env
      VBool False -> evalStmtsWithEnv flowMap eBlock env
      _ -> return $ Just $ VError "Condition must be boolean"

  IRDelta{} -> error "IRDelta should be handled in evalStmts"

-- Evaluate an IR expression
evalIRExpr :: ExprIR -> Env -> IO Value
evalIRExpr expr env = case expr of
  IRNumLit n     -> return $ VNum n
  IRStrLit s     -> return $ VStr s
  IRBoolLit b    -> return $ VBool b

  IRVar name     -> case Map.lookup name env of
                      Just val -> return val
                      Nothing  -> return $ VError ("Unbound variable: " ++ name)

  IRAdd e1 e2 -> binOp (+) e1 e2 env
  IRSub e1 e2 -> binOp (-) e1 e2 env
  IRMul e1 e2 -> binOp (*) e1 e2 env
  IRDiv e1 e2 -> do
    v1 <- evalIRExpr e1 env
    v2 <- evalIRExpr e2 env
    case (v1, v2) of
      (VNum _, VNum 0) -> return $ VError "Division by zero"
      (VNum n1, VNum n2) -> return $ VNum (n1 / n2)
      _ -> return $ VError "Type error in division"

  IREquals e1 e2 -> do
    v1 <- evalIRExpr e1 env
    v2 <- evalIRExpr e2 env
    return $ VBool (v1 == v2)

  IRNot e -> do
    v <- evalIRExpr e env
    case v of
      VBool b -> return $ VBool (not b)
      _       -> return $ VError "Expected boolean in 'not'"

-- Binary arithmetic helper
binOp :: (Double -> Double -> Double) -> ExprIR -> ExprIR -> Env -> IO Value
binOp f e1 e2 env = do
  v1 <- evalIRExpr e1 env
  v2 <- evalIRExpr e2 env
  case (v1, v2) of
    (VNum n1, VNum n2) -> return $ VNum (f n1 n2)
    _ -> return $ VError "Type error in arithmetic operation"
