module RVRS.Eval where

import RVRS.AST
import qualified Data.Map as M

-- | Our environment: variable bindings
type Env = M.Map String Value

-- | Values produced by evaluating expressions
data Value
  = VNum Int
  | VBool Bool
  | VStr String
  | VUnit         -- used for now to represent 'no meaningful value'
  deriving (Show, Eq)

-- | Evaluate an expression in an environment
evalExpr :: Env -> Expr -> Maybe Value
evalExpr env (Var name) =
  M.lookup name env

evalExpr _ (NumLit n) = Just $ VNum n
evalExpr _ (BoolLit b) = Just $ VBool b
evalExpr _ (StrLit s) = Just $ VStr s

evalExpr env (Equals e1 e2) = do
  v1 <- evalExpr env e1
  v2 <- evalExpr env e2
  return $ VBool (v1 == v2)

evalExpr env (Add e1 e2) = do
  VNum x <- evalExpr env e1
  VNum y <- evalExpr env e2
  return $ VNum (x + y)

evalExpr env (Sub e1 e2) = do
  VNum x <- evalExpr env e1
  VNum y <- evalExpr env e2
  return $ VNum (x - y)

evalExpr env (Mul e1 e2) = do
  VNum x <- evalExpr env e1
  VNum y <- evalExpr env e2
  return $ VNum (x * y)

evalExpr _ _ = Nothing


evalFlow :: Flow -> IO ()
evalFlow (Flow name args body) = do
  putStrLn $ "Evaluating flow: " ++ name
  _ <- evalBody M.empty body
  return ()

evalBody :: Env -> [Statement] -> IO Env
evalBody env [] = return env
evalBody env (stmt:stmts) = do
  env' <- evalStmt env stmt
  evalBody env' stmts

evalStmt :: Env -> Statement -> IO Env
evalStmt env stmt = case stmt of
  Mouth (StrLit s) -> putStrLn ("mouth: " ++ s) >> return env
  Echo (StrLit s)  -> putStrLn ("echo: " ++ s) >> return env

  Delta var expr ->
    case evalExpr env expr of
      Just val -> return $ M.insert var val env
      Nothing  -> putStrLn ("Could not evaluate: " ++ show expr) >> return env

  Branch cond thenStmts elseStmts ->
    case evalExpr env cond of
      Just (VBool True)  -> evalBody env thenStmts
      Just (VBool False) -> evalBody env elseStmts
      Just val -> do
        putStrLn $ "Non-boolean condition in branch: " ++ show val
        return env
      Nothing -> do
        putStrLn $ "Failed to evaluate branch condition: " ++ show cond
        return env

  _ -> return env
