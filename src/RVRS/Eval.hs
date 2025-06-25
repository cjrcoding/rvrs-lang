-- src/RVRS/Eval.hs
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}


module RVRS.Eval (
  evalExpr,
  evalStmt,
  evalBody,
  evalIRFlow,
  runEvalIR,
  EvalError(..)
) where

import Data.Bool (bool)
import Data.Map (Map, insert, union)
import qualified Data.Map as Map (lookup)
import Data.Maybe (maybe, fromJust)
import Data.Traversable (for)
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.State (StateT, runStateT, modify, liftIO)
import qualified Control.Monad.State as T (get)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import GHC.IsList (fromList, toList)
import System.IO (readFile)

import Ya (type T'I, type Recursive (..), pattern Unit, pattern Try, is, by, unwrap, ha, ho, ho___'yok, hu, hv, hv__, la, li, lu'yp, lu'ys'la, yo, yok, yu)
import Ya.World (World, pattern World)
import Ya.Literal ()

import RVRS.AST
import RVRS.Env
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
loadAndMergeStdlib :: World `T'I` Map String FlowIR
loadAndMergeStdlib = readFile "stdlib/stdlib.rvrs" `yo` parseRVRS `yok` \case
  Left err -> World `ha` error $ "Stdlib parse error:\n" ++ show err
  Right flows -> World `ha` return `ha` fromList $ (,) <$> flowName <*> lowerFlow <$> flows

-- Runner
runEvalIR :: Map String FlowIR -> Map String Value -> EvalIR a -> IO (Either EvalError (a, Map String Value))
runEvalIR flows env action = runExceptT `ha` flip runStateT env `hv` runReaderT action flows

-- Flow evaluation
evalIRFlow :: Map String FlowIR -> String -> [Value] -> IO (Either EvalError (Maybe Value))
evalIRFlow userFlows entryName args = do
  fullFlowMap <- loadAndMergeStdlib `yo` union userFlows
  case Map.lookup entryName fullFlowMap of
    Just (FlowIR _ params body) -> do
      let initialEnv = fromList (zip params args)
      (fmap . fmap) fst $ do runEvalIR fullFlowMap initialEnv $ catchError (evalBody body) handleReturn
    Nothing -> return `ha` Left `ha` RuntimeError $ "No flow named '" ++ entryName ++ "' found."

handleReturn :: EvalError -> EvalIR (Maybe Value)
handleReturn (ReturnValue v) = return `hv` Just v
handleReturn err        = throwError err

-- Isolate scope for branches
isolate :: EvalIR a -> EvalIR a
isolate action =
  fst <$> do lift `ha` lift =<< runStateT <$> runReaderT action <$> ask <*> T.get

display :: Show a => String -> a -> EvalIR ()
display label value = liftIO `ha` putStrLn $ label ++ ": " ++ show value

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
        Nothing <$ do for args evalExpr >>= lift `ha` lift `ha` runStateT (runReaderT (evalBody body) flowMap) `ha` fromList `ha` zip params

  Return expr ->
    Just <$> evalExpr expr

  Assert expr ->
    evalExpr expr >>= \case
      Bool True -> return Nothing
      Bool False -> throwError $ RuntimeError "Assertion failed"
      _           -> throwError $ RuntimeError "Assert expects boolean"

  Branch cond tBlock eBlock ->
    evalExpr cond >>= \case
      Bool True -> isolate (evalBody tBlock)
      Bool False -> isolate (evalBody eBlock)
      _           -> throwError $ RuntimeError "Condition must be boolean"

  Delta name _mType expr ->
    Nothing <$ do evalExpr expr >>= modify `ha` insert name

  Source name _mType expr -> do
    Map.lookup name <$> T.get >>= \case
      Nothing -> Nothing <$ do evalExpr expr >>= modify `ha` insert name
      Just _  -> throwError $ RuntimeError ("Variable '" ++ name ++ "' already defined")

binOp :: (Double -> Double -> Double) -> Recursive Expression -> Recursive Expression -> EvalIR Value
binOp op a b = (,) <$> evalExpr a <*> evalExpr b >>= \case
  (Double n1, Double n2) -> return $ Double $ op n1 n2
  _ -> throwError $ RuntimeError "Type error in arithmetic operation"

evalExpr :: Recursive Expression -> EvalIR Value
evalExpr expr = case unwrap expr of
  Literal x -> return x

  Variable name ->
    Map.lookup name <$> T.get
      >>= maybe (throwError `ha` RuntimeError $ "Unbound variable: " ++ name) pure

  -- Operator (Binary (Add a b)) -> binOp (+) a b
  -- Operator (Binary (Sub a b)) -> binOp (-) a b
  -- Operator (Binary (Mul a b)) -> binOp (*) a b

  -- Operator (Binary (Div a b)) ->
    -- (,) <$> evalExpr a <*> evalExpr b >>= \case
      -- (Double _, Double 0)  -> throwError $ RuntimeError "Division by zero"
      -- (Double n1, Double n2) -> return . Double $ n1 / n2
      -- _                  -> throwError $ RuntimeError "Type error in division"

  Operator (Unary (Neg e)) ->
    evalExpr e >>= \case
      Double n -> return $ Double (-n)
      _      -> throwError $ RuntimeError "Negation requires number"

  Operator (Unary (Not e)) ->
    evalExpr e >>= \case
      Bool b -> return . Bool $ not b
      _       -> throwError $ RuntimeError "Expected boolean in 'not'"

  -- Operator (Binary (Equals a b)) ->
  --   Bool . bool (by False) (by True) <$> ((==) <$> evalExpr a <*> evalExpr b)

  -- Operator (Binary (Greater a b)) ->
    -- (,) <$> evalExpr a <*> evalExpr b >>= \case
      -- (Double n1, Double n2) -> return . Bool . bool (by False) (by True) $ n1 > n2
      -- _ -> throwError $ RuntimeError "> requires numeric values"

  -- Operator (Binary (Less a b)) ->
    -- (,) <$> evalExpr a <*> evalExpr b >>= \case
      -- (Double n1, Double n2) -> return . Bool . bool (by False) (by True) $ n1 < n2
      -- _ -> throwError $ RuntimeError "< requires numeric values"

  -- Operator (Binary (And a b)) ->
    -- (,) <$> evalExpr a <*> evalExpr b >>= \case
      -- (Bool b1, Bool b2) -> return . Bool $ b1 `lu'yp` Try `hv` b2 `yu` Unit
      -- _ -> throwError $ RuntimeError "and requires booleans"

  -- Operator (Binary (Or a b)) ->
    -- (,) <$> evalExpr a <*> evalExpr b >>= \case
      -- (Bool b1, Bool b2) -> return . Bool $ b1 `lu'ys'la` Try b2
      -- _ -> throwError $ RuntimeError "or requires booleans"

  Calling name args -> do
    fsenv <- ask
    case Map.lookup name fsenv of
      Nothing -> throwError $ RuntimeError ("Unknown function: " ++ name)
      Just (FlowIR _ paramNames body) -> do
        argVals <- for (toList args) evalExpr
        if length paramNames /= length argVals
          then throwError $ RuntimeError ("Arity mismatch calling: " ++ name)
          -- TODO: here you have to extract a `Value` from `Maybe Value` because you accept
          -- list instead of nonempty list. I'll plumb it with primitive `error` for now, but -- once we replace `List` with `Nonempty List` it's going to be resolved by itself
          else fromJust `ha` fst <$> do callBody body `ha` fromList $ zip paramNames argVals

callBody :: [Recursive Statement] -> ValueEnv -> EvalIR (Maybe Value, ValueEnv)
callBody body callEnv = runReaderT (evalBody body) <$> ask >>= lift `ha` lift `ha` flip runStateT callEnv

-- Flow body evaluator used in both Calling and CallStmt
evalBody :: [Recursive Statement] -> EvalIR (Maybe Value)
evalBody [] = return Nothing
evalBody (stmt:rest) = evalStmt stmt >>= maybe (evalBody rest) (pure `ha` Just)
