module RVRS.Eval.EvalStmt (evalIRStmt) where

import RVRS.Value (Value(..))
import RVRS.Eval.EvalExpr (evalExpr, evalBody)
import RVRS.Eval.Types (EvalIR, EvalError(..))
import RVRS.AST (StmtIR (..), FlowIR (..))

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
evalIRStmt :: StmtIR -> EvalIR (Maybe Value)
evalIRStmt stmt = case stmt of
  IREcho expr -> do
    val <- evalExpr expr
    liftIO $ putStrLn ("echo: " ++ show val)
    return Nothing

  IRWhisper label expr -> do
    val <- evalExpr expr
    liftIO $ putStrLn ("→ whisper: " ++ label ++ " = " ++ show val)
    return Nothing

  -- ✅ Correct: discard subflow return value
  IRCallStmt name args -> do
    flowMap <- ask
    case Map.lookup name flowMap of
      Nothing -> throwError $ RuntimeError ("Unknown flow: " ++ name)
      Just (FlowIR _ params body) ->
        Nothing <$ do for args evalExpr >>= lift . lift . runStateT (runReaderT (evalBody body) flowMap) . Map.fromList . zip params

  IRReturn expr ->
    Just <$> evalExpr expr

  IRMouth expr -> do
    val <- evalExpr expr
    liftIO $ putStrLn ("mouth: " ++ show val)
    return Nothing

  IRAssert expr ->
    evalExpr expr >>= \case
      VBool True  -> return Nothing
      VBool False -> throwError $ RuntimeError "Assertion failed"
      _           -> throwError $ RuntimeError "Assert expects boolean"

  IRBranch cond tBlock eBlock ->
    evalExpr cond >>= \case
      VBool True  -> isolate (evalBody tBlock)
      VBool False -> isolate (evalBody eBlock)
      _           -> throwError $ RuntimeError "Condition must be boolean"

  IRDelta name expr _mType ->
    Nothing <$ do evalExpr expr >>= modify . Map.insert name

  IRSource name expr _mType -> do
    Map.lookup name <$> get >>= \case
      Nothing -> Nothing <$ do evalExpr expr >>= modify . Map.insert name
      Just _  -> throwError $ RuntimeError ("Variable '" ++ name ++ "' already defined")
