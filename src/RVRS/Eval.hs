{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module RVRS.Eval where

import RVRS.AST
import qualified Data.Map as M

-- | Values produced by evaluating expressions
data Value
  = VNum Double
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
evalExpr :: M.Map String Flow -> Env -> Expr -> IO (Maybe Value)
evalExpr flowEnv env (Var name) =
  case lookupVar name env of
    Just (Mutable v)   -> return (Just v)
    Just (Immutable v) -> return (Just v)
    Nothing            -> return Nothing

evalExpr _ _ (NumLit n) = return $ Just $ VNum n
evalExpr _ _ (BoolLit b) = return $ Just $ VBool b
evalExpr _ _ (StrLit s) = return $ Just $ VStr s

evalExpr flowEnv env (Equals e1 e2) = do
  v1 <- evalExpr flowEnv env e1
  v2 <- evalExpr flowEnv env e2
  return $ VBool <$> ((==) <$> v1 <*> v2)

evalExpr flowEnv env (Add e1 e2) = do
  Just (VNum x) <- evalExpr flowEnv env e1
  Just (VNum y) <- evalExpr flowEnv env e2
  return $ Just $ VNum (x + y)

evalExpr flowEnv env (Sub e1 e2) = do
  Just (VNum x) <- evalExpr flowEnv env e1
  Just (VNum y) <- evalExpr flowEnv env e2
  return $ Just $ VNum (x - y)

evalExpr flowEnv env (Mul e1 e2) = do
  Just (VNum x) <- evalExpr flowEnv env e1
  Just (VNum y) <- evalExpr flowEnv env e2
  return $ Just $ VNum (x * y)

evalExpr flowEnv env (Div e1 e2) = do
  Just (VNum x) <- evalExpr flowEnv env e1
  Just (VNum y) <- evalExpr flowEnv env e2
  if y == 0
    then do
      putStrLn "Error: division by zero"
      return Nothing
    else return $ Just $ VNum (x / y)

evalExpr flowEnv env (Not e) = do
  Just (VBool b) <- evalExpr flowEnv env e
  return $ Just $ VBool (not b)

evalExpr flowEnv env (And e1 e2) = do
  Just (VBool b1) <- evalExpr flowEnv env e1
  Just (VBool b2) <- evalExpr flowEnv env e2
  return $ Just $ VBool (b1 && b2)

evalExpr flowEnv env (Or e1 e2) = do
  Just (VBool b1) <- evalExpr flowEnv env e1
  Just (VBool b2) <- evalExpr flowEnv env e2
  return $ Just $ VBool (b1 || b2)


evalExpr flowEnv env (CallExpr name) =
  case M.lookup name flowEnv of
    Just flow -> evalFlow flowEnv flow (pushScope env)
    Nothing -> do
      putStrLn $ "Error: flow '" ++ name ++ "' not found"
      return Nothing

evalExpr _ _ expr = error $ "Unhandled expression: " ++ show expr


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

  Echo expr -> do
    result <- evalExpr flowEnv env expr
    case result of
      Just val -> putStrLn ("echo: " ++ formatVal val) >> return (Continue env)
      Nothing  -> putStrLn "echo: [error: unsupported value]" >> return (Continue env)

  Mouth expr -> do
    result <- evalExpr flowEnv env expr
    case result of
      Just val -> putStrLn ("mouth: " ++ formatVal val) >> return (Returned val)
      Nothing  -> putStrLn "mouth: [error: unsupported value]" >> return (Returned VUnit)

  Delta var expr -> do
    result <- evalExpr flowEnv env expr
    case result of
      Just val ->
        case insertVar var (Mutable val) env of
          Left err     -> putStrLn ("Error: " ++ err) >> return (Continue env)
          Right newEnv -> return (Continue newEnv)
      Nothing -> putStrLn ("Could not evaluate: " ++ show expr) >> return (Continue env)

  Source var expr -> do
    result <- evalExpr flowEnv env expr
    case result of
      Just val -> return (Continue (insertSource var val env))
      Nothing  -> putStrLn ("Could not evaluate: " ++ show expr) >> return (Continue env)

  Branch cond thenStmts elseStmts -> do
    result <- evalExpr flowEnv env cond
    case result of
      Just (VBool True) -> do
        res <- evalBody flowEnv (pushScope env) thenStmts
        case res of
          Continue _ -> return (Continue env)  -- discard inner scope
          Returned v -> return (Returned v)
      Just (VBool False) -> do
        res <- evalBody flowEnv (pushScope env) elseStmts
        case res of
          Continue _ -> return (Continue env)
          Returned v -> return (Returned v)
      Just val -> do
        putStrLn $ "Non-boolean condition in branch: " ++ show val
        return (Continue env)
      Nothing -> do
        putStrLn $ "Failed to evaluate branch condition: " ++ show cond
        return (Continue env)

  Return expr -> do
    result <- evalExpr flowEnv env expr
    case result of
      Just val -> return (Returned val)
      Nothing  -> putStrLn ("Could not evaluate return value: " ++ show expr) >> return (Returned VUnit)

  Call name ->
    case M.lookup name flowEnv of
      Just targetFlow -> do
        result <- evalFlow flowEnv targetFlow (pushScope env)
        case result of
          Just val -> return (Returned val)
          Nothing  -> return (Continue env)
      Nothing -> do
        putStrLn $ "Error: flow '" ++ name ++ "' not found"
        return (Continue env)

  _ -> putStrLn "Unknown statement encountered." >> return (Continue env)
