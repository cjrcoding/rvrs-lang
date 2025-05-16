module RVRS.Lower where

import RVRS.AST
import RVRS.IR

-- | Convert a full Flow into IR
lowerFlow :: Flow -> FlowIR
lowerFlow (Flow name args body) =
  FlowIR name (map argName args) (map lowerStmt body)

-- | Lower an AST Statement into IR
lowerStmt :: Statement -> StmtIR
lowerStmt stmt = case stmt of
  Delta name _ expr -> IRDelta name (lowerExpr expr)
  Source name expr      -> IRSource name (lowerExpr expr)
  Echo expr             -> IREcho (lowerExpr expr)
  Whisper expr -> IRWhisper "unnamed" (lowerExpr expr)
  Mouth expr            -> IRMouth (lowerExpr expr)
  Branch cond t f       -> IRBranch (lowerExpr cond) (map lowerStmt t) (map lowerStmt f)
  Return expr           -> IRReturn (lowerExpr expr)
  Call name args        -> IRCallStmt name (map lowerExpr args)
  Assert expr           -> IRAssert (lowerExpr expr)

-- | Lower an AST Expr into IR
lowerExpr :: Expr -> ExprIR
lowerExpr expr = case expr of
  Var name         -> IRVar name
  StrLit s         -> IRStrLit s
  NumLit n         -> IRNumLit n
  BoolLit b        -> IRBoolLit b
  Add a b          -> IRAdd (lowerExpr a) (lowerExpr b)
  Sub a b          -> IRSub (lowerExpr a) (lowerExpr b)
  Mul a b          -> IRMul (lowerExpr a) (lowerExpr b)
  Div a b          -> IRDiv (lowerExpr a) (lowerExpr b)
  Neg e            -> IRNeg (lowerExpr e)
  CallExpr name es -> IRCall name (map lowerExpr es)
  Not e            -> IRNot (lowerExpr e)
  And a b          -> IRAnd (lowerExpr a) (lowerExpr b)
  Or a b           -> IROr  (lowerExpr a) (lowerExpr b)
  Equals a b       -> IREquals (lowerExpr a) (lowerExpr b)
  GreaterThan a b  -> IRGreaterThan (lowerExpr a) (lowerExpr b)
  LessThan a b     -> IRLessThan (lowerExpr a) (lowerExpr b)
