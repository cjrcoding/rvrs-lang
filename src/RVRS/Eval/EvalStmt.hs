module RVRS.Eval.EvalStmt (evalIRStmt) where

import RVRS.Value (Value(..))
import RVRS.Eval.EvalExpr (evalExpr, evalBody)
import RVRS.Eval.Types (EvalIR, EvalError(..))
import RVRS.AST (Recursive (..), Statement (..), FlowIR (..))

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.Traversable

-- Isolate scope for branches
isolate :: EvalIR a -> EvalIR a
isolate action =
  fst <$> do lift . lift =<< runStateT <$> runReaderT action <$> ask <*> get

-- Statement evaluator
evalIRStmt :: Recursive Statement -> EvalIR (Maybe Value)
evalIRStmt stmt = case stmt of
  Recursive (Echo expr) -> do
    val <- evalExpr expr
    liftIO $ putStrLn ("echo: " ++ show val)
    return Nothing

  Recursive (Whisper expr) -> do
    val <- evalExpr expr
    liftIO $ putStrLn ("→ whisper: " ++ " = " ++ show val)
    return Nothing

  -- ✅ Correct: discard subflow return value
  Recursive (Call name args) -> do
    flowMap <- ask
    case Map.lookup name flowMap of
      Nothing -> throwError $ RuntimeError ("Unknown flow: " ++ name)
      Just (FlowIR _ params body) ->
        Nothing <$ do for args evalExpr >>= lift . lift . runStateT (runReaderT (evalBody body) flowMap) . Map.fromList . zip params

  Recursive (Return expr) ->
    Just <$> evalExpr expr

  Recursive (Mouth expr) -> do
    val <- evalExpr expr
    liftIO $ putStrLn ("mouth: " ++ show val)
    return Nothing

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
