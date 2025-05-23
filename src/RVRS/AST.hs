module RVRS.AST where

import RVRS.Parser.Type (RVRSType(..))

-- | Represents a named flow of ritual logic
data Flow = Flow
  { flowName :: String           -- ^ The flow's name (e.g. "main", "add")
  , flowArgs :: [Argument]       -- ^ Named ritual arguments, optionally typed
  , flowBody :: [Statement]      -- ^ The ritual body (statements)
  } deriving (Show, Eq)

-- | A named argument to a flow, e.g., `x: Num`
data Argument = Argument
  { argName :: String            -- ^ The argument name
  , argType :: RVRSType          -- ^ The argument type
  } deriving (Show, Eq)

-- | Statements inside a flow block
data Statement
  = Source String (Maybe RVRSType) Expr         -- source x: Type = ...
  | Delta String (Maybe RVRSType) Expr          -- delta x: Type = ...
  | Branch Expr [Statement] [Statement]         -- branch cond { ... } else { ... }
  | Mouth Expr                                  -- mouth expr
  | Whisper Expr                                -- whisper expr
  | Echo Expr                                   -- echo expr
  | Pillar String Expr                          -- pillar NAME = expr
  | Return Expr
  | Call String [Expr]
  | Assert Expr
  deriving (Show, Eq)

-- | Expressions in RVRS
data Expr
  = Var String
  | StrLit String
  | BoolLit Bool
  | Equals Expr Expr
  | GreaterThan Expr Expr
  | LessThan Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | NumLit Double
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | CallExpr String [Expr]
  | Neg Expr
  deriving (Show, Eq)
