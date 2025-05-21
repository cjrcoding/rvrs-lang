-- src/RVRS/IR.hs

module RVRS.IR where


import RVRS.Parser.Type (RVRSType)


-- | Intermediate representation of a flow
data FlowIR = FlowIR
  { flowName :: String
  , flowArgs :: [String]
  , flowBody :: [StmtIR]
  } deriving (Show, Eq)

-- | Lowered statements (IR version of AST Statement)
data StmtIR
  = IRDelta String ExprIR (Maybe RVRSType)
  | IRSource String ExprIR (Maybe RVRSType)
  | IREcho ExprIR
  | IRWhisper String ExprIR
  | IRMouth ExprIR
  | IRBranch ExprIR [StmtIR] [StmtIR]
  | IRReturn ExprIR
  | IRCallStmt String [ExprIR]  -- Top-level statement like: call foo(x, y)
  | IRAssert ExprIR
  deriving (Show, Eq)

-- | Core IR expressions
data ExprIR
  = IRVar String
  | IRStrLit String
  | IRNumLit Double
  | IRBoolLit Bool

  -- Unary & binary ops
  | IRAdd ExprIR ExprIR
  | IRSub ExprIR ExprIR
  | IRMul ExprIR ExprIR
  | IRDiv ExprIR ExprIR
  | IRNeg ExprIR
  | IRNot ExprIR
  | IRAnd ExprIR ExprIR
  | IROr  ExprIR ExprIR
  | IREquals ExprIR ExprIR
  | IRGreaterThan ExprIR ExprIR
  | IRLessThan ExprIR ExprIR

  -- Function call within an expression: foo(x, y)
  | IRCallExpr String [ExprIR]
  deriving (Show, Eq)
