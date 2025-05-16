module RVRS.EvalIR (evalIRFlow, evalIRExpr, evalIRStmt) where

import RVRS.IR
import qualified Data.Map as Map
import RVRS.Value (Value(..))

type Env = Map.Map String Value

-- Entry point for running a flow
evalIRFlow :: FlowIR -> [Value] -> IO (Maybe Value)
evalIRFlow (FlowIR name args body) argVals = do
  let initialEnv = Map.fromList (zip args argVals)
  evalStmts body initialEnv

-- Evaluate a list of IR statements (handles IRDelta here)
evalStmts :: [StmtIR] -> Env -> IO (Maybe Value)
evalStmts [] _ = return Nothing
evalStmts (stmt:rest) env = case stmt of
  IRDelta name expr -> do
    val <- evalIRExpr expr env
    let newEnv = Map.insert name val env
    evalStmts rest newEnv

  _ -> do
    result <- evalIRStmt stmt env
    case result of
      Just val -> return (Just val)  -- early return short-circuits flow
      Nothing  -> evalStmts rest env

-- Evaluate a single IR statement (except Delta, handled above)
evalIRStmt :: StmtIR -> Env -> IO (Maybe Value)
evalIRStmt stmt env = case stmt of
  IREcho expr -> do
    val <- evalIRExpr expr env
    putStrLn ("echo: " ++ show val)
    return Nothing

  IRReturn expr -> do
    val <- evalIRExpr expr env
    return (Just val)

  IRBranch cond thenStmts elseStmts -> do
    condVal <- evalIRExpr cond env
    case condVal of
      VBool True  -> evalStmts thenStmts env
      VBool False -> evalStmts elseStmts env
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
