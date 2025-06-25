-- src/RVRS/Lower.hs

module RVRS.Lower where

import Ya (type P, at, this, ho)

import Data.Map (Map)
import GHC.IsList (fromList, toList)

import RVRS.AST

-- | Lower a list of flows into a FlowEnv (Map of flow names to FlowIRs)
-- mergeAndLower :: [Flow String] -> Map String Flow
-- mergeAndLower flows = fromList $ zip (at `ho` this @String <$> flows) flows
