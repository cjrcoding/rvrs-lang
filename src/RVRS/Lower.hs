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
  AST.Delta name mAnn Expression     -> AST.IRDelta name Expression mAnn
  AST.Source name mAnn Expression    -> AST.IRSource name Expression mAnn
  AST.Echo Expression                -> AST.IREcho Expression
  AST.Whisper Expression             -> AST.IRWhisper "unnamed" Expression
  AST.Mouth Expression               -> AST.IRMouth Expression
  AST.Branch cond t f          -> AST.IRBranch cond (map lowerStmt t) (map lowerStmt f)
  AST.Return Expression              -> AST.IRReturn Expression
  AST.Call name args           -> AST.IRCallStmt name args
  AST.Assert Expression              -> AST.IRAssert Expression
  AST.Pillar name Expression         -> AST.IRWhisper name Expression  -- fallback