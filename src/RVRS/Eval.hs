-- src/RVRS/Eval.hs
module RVRS.Eval (
  evalExpr,
  evalStmt,
  evalBody,
  evalIRFlow,
  runEvalIR,
  EvalError(..)
) where

import Ya (Recursive (..))
import Data.Maybe
import Data.Traversable
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import System.IO (readFile)
import qualified Data.Map as Map

import RVRS.AST
import RVRS.Env
import RVRS.Utils
import RVRS.Value
import RVRS.Parser
import RVRS.Lower

type Env     = Map.Map String Value
type FlowEnv = Map.Map String FlowIR

type EvalIR a = ReaderT FlowEnv (StateT ValueEnv (ExceptT EvalError IO)) a

data EvalError
  = RuntimeError String
  | ReturnValue Value
  deriving (Show, Eq)

-- Load stdlib and merge
loadAndMergeStdlib :: IO (Map.Map String FlowIR)
loadAndMergeStdlib = parseRVRS <$> readFile "stdlib/stdlib.rvrs" >>= \case
  Left err -> error $ "Stdlib parse error:\n" ++ show err
  Right flows -> return $ Map.fromList $ (,) <$> flowName <*> lowerFlow <$> flows

-- Runner
runEvalIR :: Map.Map String FlowIR -> Map.Map String Value -> EvalIR a -> IO (Either EvalError (a, Map.Map String Value))
runEvalIR flows env action =
  runExceptT (runStateT (runReaderT action flows) env)

-- Flow evaluation
evalIRFlow :: Map.Map String FlowIR -> String -> [Value] -> IO (Either EvalError (Maybe Value))
evalIRFlow userFlows entryName args = do
  fullFlowMap <- Map.union userFlows <$> loadAndMergeStdlib
  case Map.lookup entryName fullFlowMap of
    Just (FlowIR _ params body) -> do
      let initialEnv = Map.fromList (zip params args)
      fst <$$> do runEvalIR fullFlowMap initialEnv $ catchError (evalBody body) handleReturn
    Nothing -> return $ Left . RuntimeError $ "No flow named '" ++ entryName ++ "' found."

handleReturn :: EvalError -> EvalIR (Maybe Value)
handleReturn (ReturnValue v) = return (Just v)
handleReturn err        = throwError err

-- Isolate scope for branches
isolate :: EvalIR a -> EvalIR a
isolate action =
  fst <$> do lift . lift =<< runStateT <$> runReaderT action <$> ask <*> get


display :: Show a => String -> a -> EvalIR ()
display label value = liftIO . putStrLn $ label ++ ": " ++ show value

-- Statement evaluator
evalStmt :: Recursive Statement -> EvalIR (Maybe Value)
evalStmt stmt = case stmt of
  Recursive (Echo expr) -> Nothing <$ do evalExpr expr >>= display "echo"
  Recursive (Whisper expr) -> Nothing <$ do evalExpr expr >>= display "→ whisper: "
  Recursive (Mouth expr) -> Nothing <$ do evalExpr expr >>= display "mouth: "

  -- ✅ Correct: discard subflow return value
  Recursive (Call name args) -> do
    flowMap <- ask
    case Map.lookup name flowMap of
      Nothing -> throwError $ RuntimeError ("Unknown flow: " ++ name)
      Just (FlowIR _ params body) ->
        Nothing <$ do for args evalExpr >>= lift . lift . runStateT (runReaderT (evalBody body) flowMap) . Map.fromList . zip params

  Recursive (Return expr) ->
    Just <$> evalExpr expr

  Recursive (Assert expr) ->
    evalExpr expr >>= \case
      VBool True  -> return Nothing
      VBool False -> throwError $ RuntimeError "Assertion failed"
      _           -> throwError $ RuntimeError "Assert expects boolean"

  Recursive (Branch cond tBlock eBlock) ->
    evalExpr cond >>= \case
      VBool True  -> isolate (evalBody tBlock)
      VBool False -> isolate (evalBody eBlock)
      _           -> throwError $ RuntimeError "Condition must be boolean"

  Recursive (Delta name _mType expr) ->
    Nothing <$ do evalExpr expr >>= modify . Map.insert name

  Recursive (Source name _mType expr) -> do
    Map.lookup name <$> get >>= \case
      Nothing -> Nothing <$ do evalExpr expr >>= modify . Map.insert name
      Just _  -> throwError $ RuntimeError ("Variable '" ++ name ++ "' already defined")

binOp :: (Double -> Double -> Double) -> Recursive Expression -> Recursive Expression -> EvalIR Value
binOp op a b = (,) <$> evalExpr a <*> evalExpr b >>= \case
  (VNum n1, VNum n2) -> return $ VNum (op n1 n2)
  _ -> throwError $ RuntimeError "Type error in arithmetic operation"

evalExpr :: Recursive Expression -> EvalIR Value
evalExpr expr = case expr of
  Recursive (NumLit n)  -> return $ VNum n
  Recursive (StrLit s)  -> return $ VStr s
  Recursive (BoolLit b) -> return $ VBool b

  Recursive (Var name) ->
    Map.lookup name <$> get
      >>= maybe (throwError . RuntimeError $ "Unbound variable: " ++ name) pure

  Recursive (Add a b) -> binOp (+) a b
  Recursive (Sub a b) -> binOp (-) a b
  Recursive (Mul a b) -> binOp (*) a b

  Recursive (Div a b) ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VNum _, VNum 0)   -> throwError $ RuntimeError "Division by zero"
      (VNum n1, VNum n2) -> return $ VNum (n1 / n2)
      _                  -> throwError $ RuntimeError "Type error in division"

  Recursive (Neg e) ->
    evalExpr e >>= \case
      VNum n -> return $ VNum (-n)
      _      -> throwError $ RuntimeError "Negation requires number"

  Recursive (Not e) ->
    evalExpr e >>= \case
      VBool b -> return $ VBool (not b)
      _       -> throwError $ RuntimeError "Expected boolean in 'not'"

  Recursive (Equals a b) ->
    VBool <$> ((==) <$> evalExpr a <*> evalExpr b)

  Recursive (GreaterThan a b) ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VNum n1, VNum n2) -> return $ VBool (n1 > n2)
      _ -> throwError $ RuntimeError "> requires numeric values"

  Recursive (LessThan a b) ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VNum n1, VNum n2) -> return $ VBool (n1 < n2)
      _ -> throwError $ RuntimeError "< requires numeric values"

  Recursive (And a b) ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VBool b1, VBool b2) -> return $ VBool (b1 && b2)
      _ -> throwError $ RuntimeError "and requires booleans"

  Recursive (Or a b) ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VBool b1, VBool b2) -> return $ VBool (b1 || b2)
      _ -> throwError $ RuntimeError "or requires booleans"

  Recursive (CallExpr name args) -> do
    fsenv <- ask
    case Map.lookup name fsenv of
      Nothing -> throwError $ RuntimeError ("Unknown function: " ++ name)
      Just (FlowIR _ paramNames body) -> do
        argVals <- for args evalExpr
        if length paramNames /= length argVals
          then throwError $ RuntimeError ("Arity mismatch calling: " ++ name)
          else fromMaybe VVoid . fst <$> do callBody body . Map.fromList $ zip paramNames argVals

callBody :: [Recursive Statement] -> ValueEnv -> EvalIR (Maybe Value, ValueEnv)
callBody body callEnv = runReaderT (evalBody body) <$> ask >>= lift . lift . flip runStateT callEnv

-- Flow body evaluator used in both CallExpr and CallStmt
evalBody :: [Recursive Statement] -> EvalIR (Maybe Value)
evalBody [] = return Nothing
evalBody (stmt:rest) = evalStmt stmt >>= maybe (evalBody rest) (pure . Just)
