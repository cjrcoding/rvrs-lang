-- src/RVRS/Eval/CallFlowOnly.hs
module RVRS.Eval.CallFlowOnly (evalFlowCall) where

import RVRS.IR
import RVRS.Value
import RVRS.Eval.Types
import RVRS.Eval.SharedExpr (evalSimpleExpr)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

evalFlowCall :: String -> [Value] -> EvalIR Value
evalFlowCall name argVals = do
  flowMap <- ask
  case Map.lookup name flowMap of
    Just (FlowIR _ paramIRs body) -> do
      when (length argVals /= length paramIRs) $
        throwError $ RuntimeError "Wrong number of arguments"

      let names = map (\(ArgumentIR n _) -> n) paramIRs
          callEnv = Map.fromList (zip names argVals)

      (result, _) <- lift $ lift $ runStateT (runReaderT (evalStmts body) flowMap) callEnv
      case result of
        Just val -> return val
        Nothing  -> return VNone

    Nothing -> throwError $ RuntimeError ("Unknown flow: " ++ name)

-- inline lightweight version to avoid pulling in full EvalStmt
evalStmts :: [StmtIR] -> EvalIR (Maybe Value)
evalStmts [] = return Nothing
evalStmts (stmt:rest) = case stmt of
  IRDelta name expr _ -> do
    val <- evalSimpleExpr expr
    modify (Map.insert name val)
    evalStmts rest

  IRReturn expr -> do
    val <- evalSimpleExpr expr
    throwError (Return val)

  _ -> throwError $ RuntimeError "Unsupported statement in inline eval"
