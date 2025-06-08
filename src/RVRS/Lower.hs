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
  AST.Delta name mAnn expr     -> AST.IRDelta name (lowerExpr expr) mAnn
  AST.Source name mAnn expr    -> AST.IRSource name (lowerExpr expr) mAnn
  AST.Echo expr                -> AST.IREcho (lowerExpr expr)
  AST.Whisper expr             -> AST.IRWhisper "unnamed" (lowerExpr expr)
  AST.Mouth expr               -> AST.IRMouth (lowerExpr expr)
  AST.Branch cond t f          -> AST.IRBranch (lowerExpr cond) (map lowerStmt t) (map lowerStmt f)
  AST.Return expr              -> AST.IRReturn (lowerExpr expr)
  AST.Call name args           -> AST.IRCallStmt name (map lowerExpr args)
  AST.Assert expr              -> AST.IRAssert (lowerExpr expr)
  AST.Pillar name expr         -> AST.IRWhisper name (lowerExpr expr)  -- fallback

-- | Lower an AST Expr into IR
lowerExpr :: AST.Recursive AST.Expr -> AST.ExprIR
lowerExpr expr = case expr of
  AST.Recursive (AST.Var name)         -> AST.IRVar name
  AST.Recursive (AST.StrLit s)         -> AST.IRStrLit s
  AST.Recursive (AST.NumLit n)         -> AST.IRNumLit n
  AST.Recursive (AST.BoolLit b)        -> AST.IRBoolLit b
  AST.Recursive (AST.Add a b)          -> AST.IRAdd (lowerExpr a) (lowerExpr b)
  AST.Recursive (AST.Sub a b)          -> AST.IRSub (lowerExpr a) (lowerExpr b)
  AST.Recursive (AST.Mul a b)          -> AST.IRMul (lowerExpr a) (lowerExpr b)
  AST.Recursive (AST.Div a b)          -> AST.IRDiv (lowerExpr a) (lowerExpr b)
  AST.Recursive (AST.Neg e)            -> AST.IRNeg (lowerExpr e)
  AST.Recursive (AST.Not e)            -> AST.IRNot (lowerExpr e)
  AST.Recursive (AST.And a b)          -> AST.IRAnd (lowerExpr a) (lowerExpr b)
  AST.Recursive (AST.Or a b)           -> AST.IROr  (lowerExpr a) (lowerExpr b)
  AST.Recursive (AST.Equals a b)       -> AST.IREquals (lowerExpr a) (lowerExpr b)
  AST.Recursive (AST.GreaterThan a b)  -> AST.IRGreaterThan (lowerExpr a) (lowerExpr b)
  AST.Recursive (AST.LessThan a b)     -> AST.IRLessThan (lowerExpr a) (lowerExpr b)
  AST.Recursive (AST.CallExpr name es) -> AST.IRCallExpr name (map lowerExpr es)
