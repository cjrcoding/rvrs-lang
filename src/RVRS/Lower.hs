-- src/RVRS/Lower.hs

module RVRS.Lower (lowerFlow, mergeAndLower) where

import Data.Map (Map)
import GHC.IsList (fromList, toList)

import Ya (yo)

import RVRS.AST

-- | Lower a list of flows into a FlowEnv (Map of flow names to FlowIRs)
mergeAndLower :: [Flow] -> Map String FlowIR
mergeAndLower flows = fromList $ zip (flowName <$> flows) (lowerFlow <$> flows)

-- | Convert a full Flow into IR
lowerFlow :: Flow -> FlowIR
lowerFlow (Flow name args body) =
  FlowIR name (argName <$> args) (fromList body)

-- | Lower an AST Statement into IR
-- lowerStmt :: Recursive Statement -> Recursive Statement
