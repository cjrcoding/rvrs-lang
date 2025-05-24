module RVRS.Eval.EvalStmt (evalIRStmt, evalStmtsWithEnv) where


import RVRS.IR
import RVRS.Value (Value(..), Binding(..), valueToType, formatVal, matchesType)
import RVRS.Eval.EvalExpr (evalIRExpr)
import RVRS.Eval.Types (EvalIR, EvalError(..))
import RVRS.Parser.Type (RVRSType(..))
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

-- Isolate scope for conditionals and flow calls
isolate :: EvalIR a -> EvalIR a
isolate action = do
  env <- get
  flowMap <- ask
  let inner = runReaderT action flowMap
  result <- lift $ lift $ runStateT inner env
  return (fst result)

-- Evaluate a single statement
evalIRStmt :: StmtIR -> EvalIR (Maybe Value)
evalIRStmt stmt = case stmt of
  IREcho expr -> do
    val <- evalIRExpr expr
    liftIO $ putStrLn ("echo: " ++ show val)
    return Nothing

  IRWhisper label expr -> do
    val <- evalIRExpr expr
    liftIO $ putStrLn ("→ whisper: " ++ label ++ " = " ++ show val)
    return Nothing

  IRCallStmt name args -> do
    flowMap <- ask
    case Map.lookup name flowMap of
      Just (FlowIR _ paramIRs body) -> do
        argVals <- mapM evalIRExpr args

        -- Arity check
        when (length argVals /= length paramIRs) $
          throwError $ RuntimeError "Wrong number of arguments"

        -- Type check for arguments
        forM_ (zip paramIRs argVals) $ \(ArgumentIR _ expectedType, actualVal) ->
          unless (matchesType expectedType actualVal) $
            throwError $ RuntimeError "Argument type mismatch"

        -- Evaluate body in new scope
        let names = map (\(ArgumentIR n _) -> n) paramIRs
            callEnv = Map.fromList (zip names argVals)

        _ <- lift $ lift $ runStateT (runReaderT (evalStmtsWithEnv body) flowMap) callEnv
        return Nothing

      Nothing -> throwError $ RuntimeError ("Unknown flow: " ++ name)

  IRBranch cond tBlock eBlock -> do
    condVal <- evalIRExpr cond
    case condVal of
      VBool True  -> isolate (evalStmtsWithEnv tBlock)
      VBool False -> isolate (evalStmtsWithEnv eBlock)
      _ -> throwError $ RuntimeError "Condition must be boolean"

  IRDelta name expr mType -> do
    val <- evalIRExpr expr
    case mType of
      Just expected ->
        unless (matchesType expected val) $
          throwError $ RuntimeError ("Type mismatch in delta for " ++ name)
      Nothing -> return ()
    modify (Map.insert name val)
    return Nothing

  IRSource name expr mType -> do
    val <- evalIRExpr expr
    env <- get
    case Map.lookup name env of
      Just _ -> throwError $ RuntimeError ("Variable '" ++ name ++ "' already defined")
      Nothing -> case mType of
        Just expected ->
          unless (matchesType expected val) $
            throwError $ RuntimeError ("Type mismatch in source for " ++ name)
        Nothing -> return ()
    modify (Map.insert name val)
    return Nothing

  IRAssert expr -> do
    val <- evalIRExpr expr
    case val of
      VBool True  -> return Nothing
      VBool False -> throwError $ RuntimeError "Assertion failed"
      _           -> throwError $ RuntimeError "Assert expression must be boolean"


-- Evaluates a list of statements until return
evalStmtsWithEnv :: [StmtIR] -> EvalIR (Maybe Value)
evalStmtsWithEnv [] = return Nothing
evalStmtsWithEnv (stmt:rest) = do
  result <- evalIRStmt stmt
  case result of
    Just val -> return (Just val)
    Nothing  -> evalStmtsWithEnv rest
