{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module RVRS.Eval where

import RVRS.AST
import qualified Data.Map as M
import RVRS.Pretty (prettyExpr)


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


evalExpr flowEnv env (CallExpr name args) = do
  argVals <- mapM (evalExpr flowEnv env) args
  case M.lookup name flowEnv of
    Just (Flow _ params body) -> do
      if length argVals /= length params
        then do
          putStrLn $ "Error: argument count mismatch in call to " ++ name
          return Nothing
        else do
          let scopedArgs = zip (map argName params) argVals
              newEnv = pushScope [M.empty]
              envWithArgs = foldl (\e (k, Just v) -> insertSource k v e) newEnv scopedArgs
          result <- evalBody flowEnv envWithArgs body
          case result of
            Returned val -> return (Just val)
            Continue _   -> return Nothing
    Nothing -> do
      putStrLn $ "Error: flow '" ++ name ++ "' not found"
      return Nothing

evalExpr flowEnv env (Neg e) = do
  val <- evalExpr flowEnv env e
  case val of
    Just (VNum n) -> return $ Just $ VNum (-n)
    _             -> do
      putStrLn "Error: Cannot negate non-numeric value"
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

  Whisper expr -> do
    result <- evalExpr flowEnv env expr
    case result of
      Just val  -> putStrLn $ "→ whisper: " ++ prettyExpr expr ++ " = " ++ formatVal val
      Nothing   -> putStrLn $ "→ whisper: " ++ prettyExpr expr ++ " = [unresolved]"
    return (Continue env)

  Assert expr -> do
    result <- evalExpr flowEnv env expr
    case result of
      Just (VBool True)  -> return (Continue env)
      Just (VBool False) -> do
        putStrLn $ "Assertion failed: " ++ prettyExpr expr
        return (Continue env)
      Just val -> do
        putStrLn $ "Assertion error: non-boolean value " ++ formatVal val
        return (Continue env)
      Nothing -> do
        putStrLn $ "Assertion could not be evaluated: " ++ prettyExpr expr
        return (Continue env)

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
      Nothing -> putStrLn ("Could not evaluate: " ++ prettyExpr expr) >> return (Continue env)

  Source var expr -> do
    result <- evalExpr flowEnv env expr
    case result of
      Just val -> return (Continue (insertSource var val env))
      Nothing  -> putStrLn ("Could not evaluate: " ++ prettyExpr expr) >> return (Continue env)

  Branch cond thenStmts elseStmts -> do
    result <- evalExpr flowEnv env cond
    case result of
      Just (VBool True) -> do
        res <- evalBody flowEnv (pushScope env) thenStmts
        case res of
          Continue _ -> return (Continue env)
          Returned v -> return (Returned v)
      Just (VBool False) -> do
        res <- evalBody flowEnv (pushScope env) elseStmts
        case res of
          Continue _ -> return (Continue env)
          Returned v -> return (Returned v)
      Just val -> do
        putStrLn $ "Non-boolean condition in branch: " ++ formatVal val
        return (Continue env)
      Nothing -> do
        putStrLn $ "Failed to evaluate branch condition: " ++ prettyExpr cond
        return (Continue env)

  Return expr -> do
    result <- evalExpr flowEnv env expr
    case result of
      Just val -> return (Returned val)
      Nothing  -> putStrLn ("Could not evaluate return value: " ++ prettyExpr expr) >> return (Returned VUnit)

  Call name args -> case M.lookup name flowEnv of
    Just (Flow _ params body) -> do
      argVals <- mapM (evalExpr flowEnv env) args
      let paramNames = map argName params
      if length argVals /= length paramNames
        then do
          putStrLn $ "Error: argument count mismatch in call to " ++ name
          return (Continue env)
        else do
          let bindings = zip paramNames argVals
              newEnv = pushScope [M.empty]
              envWithArgs = foldl (\e (k, Just v) -> insertSource k v e) newEnv bindings
          result <- evalBody flowEnv envWithArgs body
          case result of
            Returned val -> return (Returned val)
            Continue _   -> return (Continue env)
    Nothing -> do
      putStrLn $ "Error: flow '" ++ name ++ "' not found"
      return (Continue env)



  _ -> putStrLn "Unknown statement encountered." >> return (Continue env)
