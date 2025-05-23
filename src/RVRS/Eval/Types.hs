-- src/RVRS/Eval/Types.hs
module RVRS.Eval.Types (EvalIR, EvalError(..)) where

import RVRS.Env (ValueEnv)
import RVRS.Value (Value(..))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import RVRS.IR (FlowIR)

type Env     = Map.Map String Value
type FlowEnv = Map.Map String FlowIR

type EvalIR a = ReaderT FlowEnv (StateT ValueEnv (ExceptT EvalError IO)) a


data EvalError
  = RuntimeError String
  | Return Value
  deriving (Show, Eq)
