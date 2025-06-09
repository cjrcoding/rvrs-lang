-- src/RVRS/Lower.hs

module RVRS.Lower (lowerFlow, mergeAndLower) where

import RVRS.AST
import qualified Data.Map as M

-- | Lower a list of flows into a FlowEnv (Map of flow names to FlowIRs)
mergeAndLower :: [Flow] -> M.Map String FlowIR
mergeAndLower flows = M.fromList $ zip (flowName <$> flows) (lowerFlow <$> flows)

-- | Convert a full Flow into IR
lowerFlow :: Flow -> FlowIR
lowerFlow (Flow name args body) =
  FlowIR name (argName <$> args) body

-- | Lower an AST Statement into IR
-- lowerStmt :: Recursive Statement -> Recursive Statement
