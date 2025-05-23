-- src/RVRS/Lower.hs

module RVRS.Lower (lowerFlow, mergeAndLower) where

import qualified RVRS.AST as AST
import qualified RVRS.IR as IR
import qualified Data.Map as M

-- | Lower a list of flows into a FlowEnv (Map of flow names to FlowIRs)
mergeAndLower :: [AST.Flow] -> M.Map String IR.FlowIR
mergeAndLower flows =
  let lowered = map lowerFlow flows
      names = map AST.flowName flows
  in M.fromList (zip names lowered)

-- | Convert a full Flow into IR
lowerFlow :: AST.Flow -> IR.FlowIR
lowerFlow (AST.Flow name args body) =
  IR.FlowIR name (map AST.argName args) (map lowerStmt body)

-- | Lower an AST Statement into IR
lowerStmt :: AST.Statement -> IR.StmtIR
lowerStmt stmt = case stmt of
  AST.Delta name mAnn expr     -> IR.IRDelta name (lowerExpr expr) mAnn
  AST.Source name mAnn expr    -> IR.IRSource name (lowerExpr expr) mAnn
  AST.Echo expr                -> IR.IREcho (lowerExpr expr)
  AST.Whisper expr             -> IR.IRWhisper "unnamed" (lowerExpr expr)
  AST.Mouth expr               -> IR.IRMouth (lowerExpr expr)
  AST.Branch cond t f          -> IR.IRBranch (lowerExpr cond) (map lowerStmt t) (map lowerStmt f)
  AST.Return expr              -> IR.IRReturn (lowerExpr expr)
  AST.Call name args           -> IR.IRCallStmt name (map lowerExpr args)
  AST.Assert expr              -> IR.IRAssert (lowerExpr expr)
  AST.Pillar name expr         -> IR.IRWhisper name (lowerExpr expr)  -- fallback

-- | Lower an AST Expr into IR
lowerExpr :: AST.Expr -> IR.ExprIR
lowerExpr expr = case expr of
  AST.Var name         -> IR.IRVar name
  AST.StrLit s         -> IR.IRStrLit s
  AST.NumLit n         -> IR.IRNumLit n
  AST.BoolLit b        -> IR.IRBoolLit b
  AST.Add a b          -> IR.IRAdd (lowerExpr a) (lowerExpr b)
  AST.Sub a b          -> IR.IRSub (lowerExpr a) (lowerExpr b)
  AST.Mul a b          -> IR.IRMul (lowerExpr a) (lowerExpr b)
  AST.Div a b          -> IR.IRDiv (lowerExpr a) (lowerExpr b)
  AST.Neg e            -> IR.IRNeg (lowerExpr e)
  AST.Not e            -> IR.IRNot (lowerExpr e)
  AST.And a b          -> IR.IRAnd (lowerExpr a) (lowerExpr b)
  AST.Or a b           -> IR.IROr  (lowerExpr a) (lowerExpr b)
  AST.Equals a b       -> IR.IREquals (lowerExpr a) (lowerExpr b)
  AST.GreaterThan a b  -> IR.IRGreaterThan (lowerExpr a) (lowerExpr b)
  AST.LessThan a b     -> IR.IRLessThan (lowerExpr a) (lowerExpr b)
  AST.CallExpr name es -> IR.IRCallExpr name (map lowerExpr es)
