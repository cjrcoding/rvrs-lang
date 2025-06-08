-- src/RVRS/Lower.hs

module RVRS.Lower (lowerFlow, mergeAndLower) where

import qualified RVRS.AST as AST
import qualified Data.Map as M

-- | Lower a list of flows into a FlowEnv (Map of flow names to FlowIRs)
mergeAndLower :: [AST.Flow] -> M.Map String AST.FlowIR
mergeAndLower flows =
  M.fromList $ zip
    (AST.flowName <$> flows) 
    (lowerFlow <$> flows)

-- | Convert a full Flow into IR
lowerFlow :: AST.Flow -> AST.FlowIR
lowerFlow (AST.Flow name args body) =
  AST.FlowIR name (AST.argName <$> args) (lowerStmt <$> body)

-- | Lower an AST Statement into IR
lowerStmt :: AST.Statement -> AST.StmtIR
lowerStmt stmt = case stmt of
  AST.Delta name mAnn expr     -> AST.IRDelta name expr mAnn
  AST.Source name mAnn expr    -> AST.IRSource name expr mAnn
  AST.Echo expr                -> AST.IREcho expr
  AST.Whisper expr             -> AST.IRWhisper "unnamed" expr
  AST.Mouth expr               -> AST.IRMouth expr
  AST.Branch cond t f          -> AST.IRBranch cond (map lowerStmt t) (map lowerStmt f)
  AST.Return expr              -> AST.IRReturn expr
  AST.Call name args           -> AST.IRCallStmt name args
  AST.Assert expr              -> AST.IRAssert expr
  AST.Pillar name expr         -> AST.IRWhisper name expr  -- fallback