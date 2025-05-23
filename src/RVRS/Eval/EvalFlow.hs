module RVRS.Eval.EvalFlow (evalIRFlow, runEvalIR, EvalError(..)) where

import RVRS.IR
import RVRS.Value (Value(..))
import RVRS.Eval.EvalStmt (evalStmtsWithEnv)
import RVRS.Eval.Types (EvalIR, EvalError(..))  
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import System.IO (readFile)
import RVRS.Parser (parseRVRS)
import RVRS.Lower (lowerFlow)
import qualified RVRS.AST as AST

-- Load stdlib and merge
loadAndMergeStdlib :: IO (Map.Map String FlowIR)
loadAndMergeStdlib = do
  src <- readFile "stdlib/stdlib.rvrs"
  case parseRVRS src of
    Left err -> error $ "Stdlib parse error:\n" ++ show err
    Right flows -> return $ Map.fromList [(AST.flowName f, lowerFlow f) | f <- flows]

-- Runner
runEvalIR :: Map.Map String FlowIR -> Map.Map String Value -> EvalIR a -> IO (Either EvalError (a, Map.Map String Value))
runEvalIR flows env action =
  runExceptT (runStateT (runReaderT action flows) env)

-- Flow evaluation
evalIRFlow :: Map.Map String FlowIR -> String -> [Value] -> IO (Either EvalError (Maybe Value))
evalIRFlow userFlows entryName args = do
  stdlibFlows <- loadAndMergeStdlib
  let fullFlowMap = Map.union userFlows stdlibFlows
  case Map.lookup entryName fullFlowMap of
    Just (FlowIR _ params body) -> do
      let initialEnv = Map.fromList (zip params args)
      result <- runEvalIR fullFlowMap initialEnv $ catchError (evalStmtsWithEnv body) handleReturn
      return $ fmap fst result
    Nothing -> return $ Left (RuntimeError $ "No flow named '" ++ entryName ++ "' found.")
  where
    handleReturn :: EvalError -> EvalIR (Maybe Value)
    handleReturn (Return v) = return (Just v)
    handleReturn err        = throwError err
