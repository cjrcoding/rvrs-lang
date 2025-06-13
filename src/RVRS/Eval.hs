-- src/RVRS/Eval.hs
module RVRS.Eval (
  evalExpr,
  evalStmt,
  evalBody,
  evalIRFlow,
  runEvalIR,
  EvalError(..)
) where

import Data.Map (Map, insert, union)
import qualified Data.Map as Map (lookup)
import Data.Maybe
import Data.Traversable
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import GHC.IsList (fromList)
import System.IO (readFile)

import Ya (Recursive (..), is, unwrap, ho, ha, hv__, la, li)

import RVRS.AST
import RVRS.Env
import RVRS.Utils
import RVRS.Value
import RVRS.Parser
import RVRS.Lower

type Env     = Map String Value
type FlowEnv = Map String FlowIR

type EvalIR a = ReaderT FlowEnv (StateT ValueEnv (ExceptT EvalError IO)) a

data EvalError
  = RuntimeError String
  | ReturnValue Value
  deriving (Show, Eq)

-- Load stdlib and merge
loadAndMergeStdlib :: IO (Map String FlowIR)
loadAndMergeStdlib = parseRVRS <$> readFile "stdlib/stdlib.rvrs" >>= \case
  Left err -> error $ "Stdlib parse error:\n" ++ show err
  Right flows -> return . fromList $ (,) <$> flowName <*> lowerFlow <$> flows

-- Runner
runEvalIR :: Map String FlowIR -> Map String Value -> EvalIR a -> IO (Either EvalError (a, Map String Value))
runEvalIR flows env action =
  runExceptT (runStateT (runReaderT action flows) env)

-- Flow evaluation
evalIRFlow :: Map String FlowIR -> String -> [Value] -> IO (Either EvalError (Maybe Value))
evalIRFlow userFlows entryName args = do
  fullFlowMap <- union userFlows <$> loadAndMergeStdlib
  case Map.lookup entryName fullFlowMap of
    Just (FlowIR _ params body) -> do
      let initialEnv = fromList (zip params args)
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
evalStmt stmt = case unwrap stmt of
  Echo expr -> Nothing <$ do evalExpr expr >>= display "echo"
  Whisper expr -> Nothing <$ do evalExpr expr >>= display "→ whisper: "
  Mouth expr -> Nothing <$ do evalExpr expr >>= display "mouth: "

  -- ✅ Correct: discard subflow return value
  Call name args -> do
    flowMap <- ask
    case Map.lookup name flowMap of
      Nothing -> throwError $ RuntimeError ("Unknown flow: " ++ name)
      Just (FlowIR _ params body) ->
        Nothing <$ do for args evalExpr >>= lift . lift . runStateT (runReaderT (evalBody body) flowMap) . fromList . zip params

  Return expr ->
    Just <$> evalExpr expr

  Assert expr ->
    evalExpr expr >>= \case
      VPrim (Bool True)  -> return Nothing
      VPrim (Bool False) -> throwError $ RuntimeError "Assertion failed"
      _           -> throwError $ RuntimeError "Assert expects boolean"

  Branch cond tBlock eBlock ->
    evalExpr cond >>= \case
      VPrim (Bool True)  -> isolate (evalBody tBlock)
      VPrim (Bool False) -> isolate (evalBody eBlock)
      _           -> throwError $ RuntimeError "Condition must be boolean"

  Delta name _mType expr ->
    Nothing <$ do evalExpr expr >>= modify . insert name

  Source name _mType expr -> do
    Map.lookup name <$> get >>= \case
      Nothing -> Nothing <$ do evalExpr expr >>= modify . insert name
      Just _  -> throwError $ RuntimeError ("Variable '" ++ name ++ "' already defined")

binOp :: (Double -> Double -> Double) -> Recursive Expression -> Recursive Expression -> EvalIR Value
binOp op a b = (,) <$> evalExpr a <*> evalExpr b >>= \case
  (VPrim (Double n1), VPrim (Double n2)) -> return $ VPrim . Double $ op n1 n2
  _ -> throwError $ RuntimeError "Type error in arithmetic operation"

evalExpr :: Recursive Expression -> EvalIR Value
evalExpr expr = case unwrap expr of
  Lit x -> return `ha` VPrim `hv__` is @String `ho` String `la` is @Double `ho` Double `la` is @Bool `ho` Bool `li` x

  Var name ->
    Map.lookup name <$> get
      >>= maybe (throwError . RuntimeError $ "Unbound variable: " ++ name) pure

  Add a b -> binOp (+) a b
  Sub a b -> binOp (-) a b
  Mul a b -> binOp (*) a b

  Div a b ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VPrim (Double _), VPrim (Double 0))  -> throwError $ RuntimeError "Division by zero"
      (VPrim (Double n1), VPrim (Double n2)) -> return . VPrim . Double $ n1 / n2
      _                  -> throwError $ RuntimeError "Type error in division"

  Neg e ->
    evalExpr e >>= \case
      VPrim (Double n) -> return . VPrim $ Double (-n)
      _      -> throwError $ RuntimeError "Negation requires number"

  Not e ->
    evalExpr e >>= \case
      VPrim (Bool b) -> return . VPrim . Bool $ not b
      _       -> throwError $ RuntimeError "Expected boolean in 'not'"

  Equals a b ->
    VPrim . Bool <$> ((==) <$> evalExpr a <*> evalExpr b)

  GreaterThan a b ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VPrim (Double n1), VPrim (Double n2)) -> return . VPrim . Bool $ n1 > n2
      _ -> throwError $ RuntimeError "> requires numeric values"

  LessThan a b ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VPrim (Double n1), VPrim (Double n2)) -> return . VPrim . Bool $ n1 < n2
      _ -> throwError $ RuntimeError "< requires numeric values"

  And a b ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VPrim (Bool b1), VPrim (Bool b2)) -> return . VPrim . Bool $ b1 && b2
      _ -> throwError $ RuntimeError "and requires booleans"

  Or a b ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (VPrim (Bool b1), VPrim (Bool b2)) -> return . VPrim . Bool $ b1 || b2
      _ -> throwError $ RuntimeError "or requires booleans"

  CallExpr name args -> do
    fsenv <- ask
    case Map.lookup name fsenv of
      Nothing -> throwError $ RuntimeError ("Unknown function: " ++ name)
      Just (FlowIR _ paramNames body) -> do
        argVals <- for args evalExpr
        if length paramNames /= length argVals
          then throwError $ RuntimeError ("Arity mismatch calling: " ++ name)
          else fromMaybe VVoid . fst <$> do callBody body . fromList $ zip paramNames argVals

callBody :: [Recursive Statement] -> ValueEnv -> EvalIR (Maybe Value, ValueEnv)
callBody body callEnv = runReaderT (evalBody body) <$> ask >>= lift . lift . flip runStateT callEnv

-- Flow body evaluator used in both CallExpr and CallStmt
evalBody :: [Recursive Statement] -> EvalIR (Maybe Value)
evalBody [] = return Nothing
evalBody (stmt:rest) = evalStmt stmt >>= maybe (evalBody rest) (pure . Just)
