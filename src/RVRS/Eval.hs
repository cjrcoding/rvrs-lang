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

import Data.Map (Map, insert, union)
import qualified Data.Map as Map (lookup)
import Data.Maybe (maybe, fromJust)
import Data.Traversable (for)
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.State (StateT, runStateT, get, modify, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import GHC.IsList (fromList)
import System.IO (readFile)

import Ya (
    type T'I, type JNT, type AR, type AR__, type Given,
    type Error, type State, type Recursive (..), type List, type Nonempty,
    pattern Unit, pattern Given, pattern Run, pattern Only, pattern None,
    pattern Error, pattern State, pattern Event, pattern Old,
    intro, unwrap, ha, ha__, ho, hu, hv, hv_, hv__, la, is, li, yi, yo, yok, yok_, yuk_
    )
import qualified Ya as Y
import Ya.World (World, pattern World)
import Ya.Conversion (may)


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
      fst <$$> do runEvalIR fullFlowMap initialEnv $ catchError (evalBody body) handleReturn
    Nothing -> return `ha` Left `ha` RuntimeError $ "No flow named '" ++ entryName ++ "' found."

handleReturn :: EvalError -> EvalIR (Maybe Value)
handleReturn (ReturnValue v) = return `hv` Just v
handleReturn err        = throwError err

-- Isolate scope for branches
isolate :: EvalIR a -> EvalIR a
isolate action =
  fst <$> do lift `ha` lift =<< runStateT <$> runReaderT action <$> ask <*> get

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
      Bool True  -> return Nothing
      Bool False -> throwError $ RuntimeError "Assertion failed"
      _           -> throwError $ RuntimeError "Assert expects boolean"

  Branch cond tBlock eBlock ->
    evalExpr cond >>= \case
      Bool True  -> isolate (evalBody tBlock)
      Bool False -> isolate (evalBody eBlock)
      _           -> throwError $ RuntimeError "Condition must be boolean"

  Delta name _mType expr ->
    Nothing <$ do evalExpr expr >>= modify `ha` insert name

  Source name _mType expr -> do
    Map.lookup name <$> get >>= \case
      Nothing -> Nothing <$ do evalExpr expr >>= modify `ha` insert name
      Just _  -> throwError $ RuntimeError ("Variable '" ++ name ++ "' already defined")

binOp :: (Double -> Double -> Double) -> Recursive Expression -> Recursive Expression -> EvalIR Value
binOp op a b = (,) <$> evalExpr a <*> evalExpr b >>= \case
  (Double n1, Double n2) -> return $ Double $ op n1 n2
  _ -> throwError $ RuntimeError "Type error in arithmetic operation"

evalExpr :: Recursive Expression -> EvalIR Value
evalExpr expr = case unwrap expr of
  Lit x -> return `hv__` is @String `ho` String `la` is @Double `ho` Double `la` is @Bool `ho` Bool `li` x

  Var name ->
    Map.lookup name <$> get
      >>= maybe (throwError `ha` RuntimeError $ "Unbound variable: " ++ name) pure

  Add a b -> binOp (+) a b
  Sub a b -> binOp (-) a b
  Mul a b -> binOp (*) a b

  Div a b ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (Double _, Double 0)  -> throwError $ RuntimeError "Division by zero"
      (Double n1, Double n2) -> return . Double $ n1 / n2
      _                  -> throwError $ RuntimeError "Type error in division"

  Neg e ->
    evalExpr e >>= \case
      Double n -> return $ Double (-n)
      _      -> throwError $ RuntimeError "Negation requires number"

  Not e ->
    evalExpr e >>= \case
      Bool b -> return . Bool $ not b
      _       -> throwError $ RuntimeError "Expected boolean in 'not'"

  Equals a b ->
    Bool <$> ((==) <$> evalExpr a <*> evalExpr b)

  GreaterThan a b ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (Double n1, Double n2) -> return . Bool $ n1 > n2
      _ -> throwError $ RuntimeError "> requires numeric values"

  LessThan a b ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (Double n1, Double n2) -> return . Bool $ n1 < n2
      _ -> throwError $ RuntimeError "< requires numeric values"

  And a b ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (Bool b1, Bool b2) -> return . Bool $ b1 && b2
      _ -> throwError $ RuntimeError "and requires booleans"

  Or a b ->
    (,) <$> evalExpr a <*> evalExpr b >>= \case
      (Bool b1, Bool b2) -> return . Bool $ b1 || b2
      _ -> throwError $ RuntimeError "or requires booleans"

  CallExpr name args -> do
    fsenv <- ask
    case Map.lookup name fsenv of
      Nothing -> throwError $ RuntimeError ("Unknown function: " ++ name)
      Just (FlowIR _ paramNames body) -> do
        argVals <- for args evalExpr
        if length paramNames /= length argVals
          then throwError $ RuntimeError ("Arity mismatch calling: " ++ name)
          -- TODO: here you have to extract a `Value` from `Maybe Value` because you accept
          -- list instead of nonempty list. I'll plumb it with primitive `error` for now, but -- once we replace `List` with `Nonempty List` it's going to be resolved by itself
          else fromJust `ha` fst <$> do callBody body `ha` fromList $ zip paramNames argVals

callBody :: [Recursive Statement] -> ValueEnv -> EvalIR (Maybe Value, ValueEnv)
callBody body callEnv = runReaderT (evalBody body) <$> ask >>= lift `ha` lift `ha` flip runStateT callEnv

-- Flow body evaluator used in both CallExpr and CallStmt
evalBody :: [Recursive Statement] -> EvalIR (Maybe Value)
evalBody [] = return Nothing
evalBody (stmt:rest) = evalStmt stmt >>= maybe (evalBody rest) (pure `ha` Just)

-- evalBody' stmts = fromList @(Nonempty List `T'I` Recursive Statement) stmts
 -- `yokl`Forth `ha` Try `ha`Maybe `ha` not `ha` may `ha` evalStmt

type Engine = Given FlowEnv `JNT` State ValueEnv `JNT` Error EvalError `JNT` World

evalStmt' :: Recursive Statement `AR__` Engine `T'I` Maybe Value
evalStmt' stmt = case unwrap stmt of
  -- Echo expr -> Nothing <$ do evalExpr expr >>= display "echo"
  -- Whisper expr -> Nothing <$ do evalExpr expr >>= display "→ whisper: "
  -- Mouth expr -> Nothing <$ do evalExpr expr >>= display "mouth: "

-- evalExpr' expr = case unwrap expr of
--   NumLit n -> intro `hv` VNum n
--   StrLit s -> intro `hv` VStr s
--   BoolLit b -> intro `hv` VBool b

  -- Var name -> intro `hv` Unit
  --  `yuk_` Run `hv__` Old `ha` State `ha` Event `hv` Y.get `yo` Map.lookup name `ho` may
  --  `yok_` Run `ha__` None `hu` Error (RuntimeError $ "Unbound variable: " ++ name) `la` intro
  --  `yok_` Run `ha__` intro @Engine @(AR)
