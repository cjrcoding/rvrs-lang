{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module RVRS.Eval.Eval where

-- Internal modules
import RVRS.AST
import RVRS.Pretty        (prettyExpr)
import RVRS.Parser.Type   (RVRSType(..))
import RVRS.Value (Value(..), Binding(..), valueToType, formatVal)

-- External libraries
import qualified Data.Map as M
import Control.Applicative (liftA2)


type Env     = [M.Map String Binding]
type FlowEnv = M.Map String Flow

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
  | M.member name scope
    = Left $ "Variable '" ++ name ++ "' already declared in this scope"
  | otherwise
    = Right $ M.insert name binding scope : rest

insertSource :: String -> Value -> Env -> Env
insertSource name val (scope:rest) = M.insert name (Immutable val) scope : rest
insertSource _ _ [] = error "No scope to insert source"


-- === Eval Expression ===
evalExpr :: FlowEnv -> Env -> Expr -> IO (Maybe Value)
evalExpr flowEnv env expr = case expr of
  Var name ->
    case lookupVar name env of
      Just (Mutable v)   -> return (Just v)
      Just (Immutable v) -> return (Just v)
      Nothing            -> return Nothing

  NumLit n      -> return $ Just $ VNum n
  BoolLit b     -> return $ Just $ VBool b
  StrLit s      -> return $ Just $ VStr s

  Equals e1 e2     -> liftBinEq (==) flowEnv env e1 e2
  GreaterThan e1 e2-> liftNumComp (>) flowEnv env e1 e2
  LessThan e1 e2   -> liftNumComp (<) flowEnv env e1 e2

  Add e1 e2  -> liftNumOp (+) flowEnv env e1 e2
  Sub e1 e2  -> liftNumOp (-) flowEnv env e1 e2
  Mul e1 e2  -> liftNumOp (*) flowEnv env e1 e2
  Div e1 e2  -> do
    Just (VNum x) <- evalExpr flowEnv env e1
    Just (VNum y) <- evalExpr flowEnv env e2
    if y == 0
      then putStrLn "Error: division by zero" >> return Nothing
      else return $ Just $ VNum (x / y)

  Not e       -> do
    Just (VBool b) <- evalExpr flowEnv env e
    return $ Just $ VBool (not b)

  And e1 e2   -> liftBoolOp (&&) flowEnv env e1 e2
  Or  e1 e2   -> liftBoolOp (||) flowEnv env e1 e2

  Neg e       -> do
    val <- evalExpr flowEnv env e
    case val of
      Just (VNum n) -> return $ Just $ VNum (-n)
      _             -> putStrLn "Error: Cannot negate non-numeric value" >> return Nothing

  CallExpr name args -> do
    argVals <- mapM (evalExpr flowEnv env) args
    case M.lookup name flowEnv of
      Just (Flow _ params body)
        | length argVals /= length params
          -> putStrLn ("Error: argument count mismatch in call to " ++ name)
             >> return Nothing
        | otherwise -> do
            let bindings    = zip (map argName params) argVals
                envWithArgs = foldl (\e (k, Just v) -> insertSource k v e) (pushScope [M.empty]) bindings
            result <- evalBody flowEnv envWithArgs body
            case result of
              Returned val -> return (Just val)
              Continue _   -> return Nothing
      Nothing -> putStrLn ("Error: flow '" ++ name ++ "' not found") >> return Nothing

liftNumComp :: (Double -> Double -> Bool) -> FlowEnv -> Env -> Expr -> Expr -> IO (Maybe Value)
liftNumComp op flowEnv env e1 e2 = do
  v1 <- evalExpr flowEnv env e1
  v2 <- evalExpr flowEnv env e2
  case (v1, v2) of
    (Just (VNum x), Just (VNum y)) -> return $ Just $ VBool (op x y)
    _ -> putStrLn "Error: expected numeric values in comparison" >> return Nothing

liftNumOp :: (Double -> Double -> Double) -> FlowEnv -> Env -> Expr -> Expr -> IO (Maybe Value)
liftNumOp op flowEnv env e1 e2 = do
  Just (VNum x) <- evalExpr flowEnv env e1
  Just (VNum y) <- evalExpr flowEnv env e2
  return $ Just $ VNum (op x y)

liftBoolOp :: (Bool -> Bool -> Bool) -> FlowEnv -> Env -> Expr -> Expr -> IO (Maybe Value)
liftBoolOp op flowEnv env e1 e2 = do
  Just (VBool x) <- evalExpr flowEnv env e1
  Just (VBool y) <- evalExpr flowEnv env e2
  return $ Just $ VBool (op x y)

liftBinEq :: (Value -> Value -> Bool) -> FlowEnv -> Env -> Expr -> Expr -> IO (Maybe Value)
liftBinEq op flowEnv env e1 e2 = do
  v1 <- evalExpr flowEnv env e1
  v2 <- evalExpr flowEnv env e2
  return $ Just $ VBool $ maybe False (uncurry op) ((,) <$> v1 <*> v2)

-- === Eval Control ===
data ExecResult = Continue Env | Returned Value

evalFlow :: FlowEnv -> Flow -> Env -> IO (Maybe Value)
evalFlow flowEnv (Flow name _ body) env = do
  putStrLn $ "Evaluating flow: " ++ name
  result <- evalBody flowEnv env body
  return $ case result of
    Returned val -> Just val
    Continue _   -> Nothing

evalBody :: FlowEnv -> Env -> [Statement] -> IO ExecResult
evalBody _ env [] = return (Continue env)
evalBody flowEnv env (stmt:stmts) = do
  result <- evalStmt flowEnv env stmt
  case result of
    Continue newEnv -> evalBody flowEnv newEnv stmts
    Returned val    -> return (Returned val)

stripScope :: ExecResult -> Env -> ExecResult
stripScope (Continue _) outer = Continue outer
stripScope (Returned v) _     = Returned v

-- === Eval Dispatcher ===
evalStmt :: FlowEnv -> Env -> Statement -> IO ExecResult
evalStmt flowEnv env = \case
  Echo e            -> evalEcho flowEnv env e
  Whisper e         -> evalWhisper flowEnv env e
  Assert e          -> evalAssert flowEnv env e
  Mouth e           -> evalMouth flowEnv env e
  Delta var ann e   -> evalDelta flowEnv env var ann e
  Source var ann e  -> evalSource flowEnv env var ann e
  Branch c t f      -> evalBranch flowEnv env c t f
  Return e          -> evalReturn flowEnv env e
  Call n args       -> evalCall flowEnv env n args

-- === Eval Statement Implementations ===
evalEcho :: FlowEnv -> Env -> Expr -> IO ExecResult
evalEcho flowEnv env expr = do
  result <- evalExpr flowEnv env expr
  case result of
    Just val -> putStrLn ("echo: " ++ formatVal val)
    Nothing  -> putStrLn "echo: [error: unsupported value]"
  return (Continue env)

evalWhisper :: FlowEnv -> Env -> Expr -> IO ExecResult
evalWhisper flowEnv env expr = do
  result <- evalExpr flowEnv env expr
  putStrLn $ case result of
    Just val -> "→ whisper: " ++ prettyExpr expr ++ " = " ++ formatVal val
    Nothing  -> "→ whisper: " ++ prettyExpr expr ++ " = [unresolved]"
  return (Continue env)

evalAssert :: FlowEnv -> Env -> Expr -> IO ExecResult
evalAssert flowEnv env expr = do
  result <- evalExpr flowEnv env expr
  case result of
    Just (VBool True)  -> return (Continue env)
    Just (VBool False) -> putStrLn ("Assertion failed: " ++ prettyExpr expr) >> return (Continue env)
    Just val           -> putStrLn ("Assertion error: non-boolean value " ++ formatVal val) >> return (Continue env)
    Nothing            -> putStrLn ("Assertion could not be evaluated: " ++ prettyExpr expr) >> return (Continue env)

evalMouth :: FlowEnv -> Env -> Expr -> IO ExecResult
evalMouth flowEnv env expr = do
  result <- evalExpr flowEnv env expr
  case result of
    Just val -> putStrLn ("mouth: " ++ formatVal val) >> return (Returned val)
    Nothing  -> putStrLn "mouth: [error: unsupported value]" >> return (Returned VUnit)

evalDelta :: FlowEnv -> Env -> String -> Maybe RVRSType -> Expr -> IO ExecResult
evalDelta flowEnv env var mAnn expr = do
  mval <- evalExpr flowEnv env expr
  case mval of
    Just val ->
      case mAnn of
        Just expectedType
          | valueToType val /= expectedType
            -> putStrLn
                 ("Type error in delta " ++ var ++
                  ": expected " ++ show expectedType ++
                  ", got " ++ show (valueToType val))
               >> return (Continue env)
        _ -> case insertVar var (Mutable val) env of
               Left err   -> putStrLn ("Error: " ++ err) >> return (Continue env)
               Right env' -> return (Continue env')
    Nothing -> putStrLn ("Could not evaluate: " ++ show expr) >> return (Continue env)

evalSource :: FlowEnv -> Env -> String -> Maybe RVRSType -> Expr -> IO ExecResult
evalSource flowEnv env var mAnn expr = do
  mval <- evalExpr flowEnv env expr
  case mval of
    Just val ->
      case mAnn of
        Just expectedType
          | valueToType val /= expectedType
            -> putStrLn
                 ("Type error in source " ++ var ++
                  ": expected " ++ show expectedType ++
                  ", got " ++ show (valueToType val))
               >> return (Continue env)
        _ -> return (Continue (insertSource var val env))
    Nothing -> putStrLn ("Could not evaluate: " ++ show expr) >> return (Continue env)

evalBranch :: FlowEnv -> Env -> Expr -> [Statement] -> [Statement] -> IO ExecResult
evalBranch flowEnv env cond thenStmts elseStmts = do
  result <- evalExpr flowEnv env cond
  case result of
    Just (VBool True) -> do
      let scopedEnv = pushScope env
      branchResult <- evalBody flowEnv scopedEnv thenStmts
      return $ stripScope branchResult env
    Just (VBool False) -> do
      let scopedEnv = pushScope env
      branchResult <- evalBody flowEnv scopedEnv elseStmts
      return $ stripScope branchResult env
    Just val ->
      putStrLn ("Non-boolean condition in branch: " ++ formatVal val) >> return (Continue env)
    Nothing ->
      putStrLn ("Failed to evaluate branch condition: " ++ prettyExpr cond) >> return (Continue env)

evalReturn :: FlowEnv -> Env -> Expr -> IO ExecResult
evalReturn flowEnv env expr = do
  result <- evalExpr flowEnv env expr
  case result of
    Just val -> return (Returned val)
    Nothing  -> putStrLn ("Could not evaluate return value: " ++ prettyExpr expr) >> return (Returned VUnit)

evalCall :: FlowEnv -> Env -> String -> [Expr] -> IO ExecResult
evalCall flowEnv env name args = case M.lookup name flowEnv of
  Just (Flow _ params body) -> do
    argVals <- mapM (evalExpr flowEnv env) args
    let paramNames = map argName params
    if length argVals /= length paramNames
      then putStrLn ("Error: argument count mismatch in call to " ++ name) >> return (Continue env)
      else do
        let bindings = zip paramNames argVals
            newEnv = pushScope [M.empty]
            envWithArgs = foldl (\e (k, Just v) -> insertSource k v e) newEnv bindings
        result <- evalBody flowEnv envWithArgs body
        return $ case result of
          Returned val -> Returned val
          Continue _   -> Continue env
  Nothing -> putStrLn ("Error: flow '" ++ name ++ "' not found") >> return (Continue env)
