module RVRS.IR where

-- | Intermediate representation of a flow
data FlowIR = FlowIR
  { flowName :: String
  , flowArgs :: [String]
  , flowBody :: [StmtIR]
  }
  deriving (Show, Eq)

-- | Lowered statements (IR version of AST Statement)
data StmtIR
  = IRDelta String ExprIR
  | IRSource String ExprIR
  | IREcho ExprIR
  | IRWhisper String ExprIR
  | IRMouth ExprIR
  | IRBranch ExprIR [StmtIR] [StmtIR]
  | IRReturn ExprIR
  | IRCallStmt String [ExprIR]
  | IRAssert ExprIR
  deriving (Show, Eq)

-- | Core expression forms
data ExprIR
  = IRVar String
  | IRStrLit String
  | IRNumLit Double
  | IRBoolLit Bool
  | IRAdd ExprIR ExprIR
  | IRSub ExprIR ExprIR
  | IRMul ExprIR ExprIR
  | IRDiv ExprIR ExprIR
  | IRNeg ExprIR
  | IRCall String [ExprIR]
  | IRNot ExprIR
  | IRAnd ExprIR ExprIR
  | IROr  ExprIR ExprIR
  | IREquals ExprIR ExprIR
  | IRGreaterThan ExprIR ExprIR
  | IRLessThan ExprIR ExprIR
  deriving (Show, Eq)
