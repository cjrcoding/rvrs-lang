module RVRS.Eval.EvalFlow (evalIRFlow, runEvalIR, EvalError(..)) where
import RVRS.AST (StmtIR (..), ExprIR (..), FlowIR (..))
import RVRS.Utils
import RVRS.Value (Value(..))
import RVRS.Eval.EvalExpr (evalBody)
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
loadAndMergeStdlib = parseRVRS <$> readFile "stdlib/stdlib.rvrs" >>= \case
  Left err -> error $ "Stdlib parse error:\n" ++ show err
  Right flows -> return $ Map.fromList $ (,) <$> AST.flowName <*> lowerFlow <$> flows

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
handleReturn (Return v) = return (Just v)
handleReturn err        = throwError err
