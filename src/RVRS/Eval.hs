module RVRS.Eval where

import RVRS.AST
import qualified Data.Map as M

-- | Values produced by evaluating expressions
data Value
  = VNum Int
  | VBool Bool
  | VStr String
  | VUnit         -- used for now to represent 'no meaningful value'
  deriving (Show, Eq)

-- | Variable bindings (mutable or immutable)
data Binding
  = Mutable Value
  | Immutable Value
  deriving (Show, Eq)

-- | Environment is now a stack of scopes
-- Head is the innermost (most recent) scope
type Env = [M.Map String Binding]

-- === Scope Utilities ===
pushScope :: Env -> Env
pushScope env = M.empty : env

lookupVar :: String -> Env -> Maybe Binding
lookupVar _ [] = Nothing
lookupVar name (scope:rest) =
  case M.lookup name scope of
    Just b  -> Just b
    Nothing -> lookupVar name rest

insertVar :: String -> Binding -> Env -> Either String Env
insertVar _ _ [] = Left "No scope to insert into"
insertVar name binding (scope:rest)
  | M.member name scope = Left $ "Variable '" ++ name ++ "' already declared in this scope"
  | otherwise            = Right $ M.insert name binding scope : rest

insertSource :: String -> Value -> Env -> Env
insertSource name val (scope:rest) = M.insert name (Immutable val) scope : rest
insertSource _ _ [] = error "No scope to insert source"

-- | Format values for echo/mouth
formatVal :: Value -> String
formatVal (VStr s)  = s
formatVal (VNum n)  = show n
formatVal (VBool b) = show b
formatVal VUnit     = "unit"

-- | Evaluate an expression in an environment
evalExpr :: Env -> Expr -> Maybe Value
evalExpr env (Var name) =
  case lookupVar name env of
    Just (Mutable v)   -> Just v
    Just (Immutable v) -> Just v
    Nothing            -> Nothing

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

-- | Execution result control flow
data ExecResult
  = Continue Env
  | Returned Value

evalFlow :: M.Map String Flow -> Flow -> Env -> IO (Maybe Value)
evalFlow flowEnv (Flow name _ body) env = do
  putStrLn $ "Evaluating flow: " ++ name
  result <- evalBody flowEnv env body
  case result of
    Returned val -> return (Just val)
    Continue _   -> return Nothing

evalBody :: M.Map String Flow -> Env -> [Statement] -> IO ExecResult
evalBody flowEnv env [] = return (Continue env)
evalBody flowEnv env (stmt:stmts) = do
  result <- evalStmt flowEnv env stmt
  case result of
    Continue newEnv -> evalBody flowEnv newEnv stmts
    Returned val    -> return (Returned val)

evalStmt :: M.Map String Flow -> Env -> Statement -> IO ExecResult
evalStmt flowEnv env stmt = case stmt of

  Echo expr ->
    case evalExpr env expr of
      Just val -> putStrLn ("echo: " ++ formatVal val) >> return (Continue env)
      Nothing  -> putStrLn "echo: [error: unsupported value]" >> return (Continue env)

  Mouth expr ->
    case evalExpr env expr of
      Just val -> putStrLn ("mouth: " ++ formatVal val) >> return (Returned val)
      Nothing  -> putStrLn "mouth: [error: unsupported value]" >> return (Returned VUnit)

  Delta var expr ->
    case evalExpr env expr of
      Just val ->
        case insertVar var (Mutable val) env of
          Left err     -> putStrLn ("Error: " ++ err) >> return (Continue env)
          Right newEnv -> return (Continue newEnv)
      Nothing -> putStrLn ("Could not evaluate: " ++ show expr) >> return (Continue env)

  Source var expr ->
    case evalExpr env expr of
      Just val -> return (Continue (insertSource var val env))
      Nothing  -> putStrLn ("Could not evaluate: " ++ show expr) >> return (Continue env)

  Branch cond thenStmts elseStmts ->
    case evalExpr env cond of
      Just (VBool True) -> do
        result <- evalBody flowEnv (pushScope env) thenStmts
        case result of
          Continue _ -> return (Continue env)  -- discard inner scope
          Returned v -> return (Returned v)
      Just (VBool False) -> do
        result <- evalBody flowEnv (pushScope env) elseStmts
        case result of
          Continue _ -> return (Continue env)
          Returned v -> return (Returned v)
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

  Call name ->
    case M.lookup name flowEnv of
    Just targetFlow -> do
      result <- evalFlow flowEnv targetFlow (pushScope env)
      case result of
        Just val -> return (Returned val)  -- 🚨 short-circuit flow
        Nothing  -> return (Continue env)
    Nothing -> do
      putStrLn $ "Error: flow '" ++ name ++ "' not found"
      return (Continue env)

  _ -> putStrLn "Unknown statement encountered." >> return (Continue env)
