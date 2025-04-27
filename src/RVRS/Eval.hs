module RVRS.Eval where

import RVRS.AST
import qualified Data.Map as M

-- | Our environment: variable bindings
data Binding
  = Mutable Value
  | Immutable Value
  deriving (Show, Eq)

type Env = M.Map String Binding

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
  case M.lookup name env of
    Just (Mutable val) -> Just val
    Just (Immutable val) -> Just val
    Nothing -> Nothing

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

evalExpr env (Not e) = do
  VBool b <- evalExpr env e
  return $ VBool (not b)

evalExpr env (And e1 e2) = do
  VBool b1 <- evalExpr env e1
  VBool b2 <- evalExpr env e2
  return $ VBool (b1 && b2)

evalExpr env (Or e1 e2) = do
  VBool b1 <- evalExpr env e1
  VBool b2 <- evalExpr env e2
  return $ VBool (b1 || b2)



evalExpr _ _ = Nothing

data ExecResult
  = Continue Env
  | Returned Value

evalFlow :: Flow -> IO (Maybe Value)
evalFlow (Flow name args body) = do
  putStrLn $ "Evaluating flow: " ++ name
  result <- evalBody M.empty body
  case result of
    Returned val -> return (Just val)
    Continue _   -> return Nothing


evalBody :: Env -> [Statement] -> IO ExecResult
evalBody env [] = return (Continue env)
evalBody env (stmt:stmts) = do
  result <- evalStmt env stmt
  case result of
    Continue newEnv -> evalBody newEnv stmts
    Returned val    -> return (Returned val)


evalStmt :: Env -> Statement -> IO ExecResult
evalStmt env stmt = case stmt of
  Echo expr -> 
    case evalExpr env expr of
      Just (VStr s) -> putStrLn ("echo: " ++ s) >> return (Continue env)
      Just (VNum n) -> putStrLn ("echo: " ++ show n) >> return (Continue env)
      Just (VBool b) -> putStrLn ("echo: " ++ show b) >> return (Continue env)
      _ -> putStrLn "echo: [error: unsupported value]" >> return (Continue env)

  Mouth expr ->
    case evalExpr env expr of
      Just (VStr s) -> putStrLn ("mouth: " ++ s) >> return (Continue env)
      Just (VNum n) -> putStrLn ("mouth: " ++ show n) >> return (Continue env)
      Just (VBool b) -> putStrLn ("mouth: " ++ show b) >> return (Continue env)
      _ -> putStrLn "mouth: [error: unsupported value]" >> return (Continue env)

  
  Delta var expr ->
    case evalExpr env expr of
    Just val ->
      case M.lookup var env of
        Just (Immutable _) -> do
          putStrLn ("Error: Cannot reassign immutable source '" ++ var ++ "'")
          return (Continue env)
        _ -> return (Continue (M.insert var (Mutable val) env))
    Nothing -> putStrLn ("Could not evaluate: " ++ show expr) >> return (Continue env)

  
  Branch cond thenStmts elseStmts ->
    case evalExpr env cond of
      Just (VBool True)  -> evalBody env thenStmts
      Just (VBool False) -> evalBody env elseStmts
      Just val -> do
        putStrLn $ "Non-boolean condition in branch: " ++ show val
        return (Continue env)
      Nothing -> do
        putStrLn $ "Failed to evaluate branch condition: " ++ show cond
        return (Continue env)
  
  Return expr ->
    case evalExpr env expr of
      Just val -> return (Returned val)
      Nothing  -> putStrLn ("Could not evaluate return value: " ++ show expr) >> return (Returned VUnit)

  Source var expr ->
    case evalExpr env expr of
      Just val -> return (Continue (insertSource var val env))
      Nothing  -> putStrLn ("Could not evaluate: " ++ show expr) >> return (Continue env)


  
  _ -> return (Continue env)

insertSource :: String -> Value -> Env -> Env
insertSource var val env = M.insert var (Immutable val) env




