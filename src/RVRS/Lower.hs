-- src/RVRS/Lower.hs

module RVRS.Lower (mergeAndLower) where

import Data.Map (Map)
import GHC.IsList (fromList, toList)

import RVRS.AST

-- | Lower a list of flows into a FlowEnv (Map of flow names to FlowIRs)
mergeAndLower :: [Flow] -> Map String Flow
mergeAndLower flows = fromList $ zip (flowName <$> flows) flows
