-- src/RVRS/Lower.hs

module RVRS.Lower (lowerFlow, mergeAndLower) where

import RVRS.AST
import qualified Data.Map as M

-- | Lower a list of flows into a FlowEnv (Map of flow names to FlowIRs)
mergeAndLower :: [Flow] -> M.Map String FlowIR
mergeAndLower flows =
  M.fromList $ zip
    (flowName <$> flows)
    (lowerFlow <$> flows)

-- | Convert a full Flow into IR
lowerFlow :: Flow -> FlowIR
lowerFlow (Flow name args body) =
  FlowIR name (argName <$> args) (body)

-- | Lower an AST Statement into IR
-- lowerStmt :: Recursive Statement -> StmtIR
-- lowerStmt stmt = case stmt of
--   Recursive (Delta name mAnn expr) -> IRDelta name expr mAnn
--   Recursive (Source name mAnn expr) -> IRSource name expr mAnn
--   Recursive (Echo expr) -> IREcho expr
--   Recursive (Whisper expr) -> IRWhisper "unnamed" expr
--   Recursive (Mouth expr) -> IRMouth expr
--   Recursive (Branch cond t f) -> IRBranch cond (map lowerStmt t) (map lowerStmt f)
--   Recursive (Return expr) -> IRReturn expr
--   Recursive (Call name args) -> IRCallStmt name args
--   Recursive (Assert expr) -> IRAssert expr
--   Recursive (Pillar name expr) -> IRWhisper name expr  -- fallback