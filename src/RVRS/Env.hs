module RVRS.Env
  ( ValueEnv
  ) where

import qualified Data.Map as Map
import RVRS.Value (Value)

-- Used only at runtime (in EvalIR)
type ValueEnv = Map.Map String Value
