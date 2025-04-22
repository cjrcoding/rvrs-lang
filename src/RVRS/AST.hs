-- src/RVRS/AST.hs

module RVRS.AST where

-- | Represents an entire RVRS flow
data Flow = Flow
  { flowName :: String
  , flowArgs :: [Argument]        -- Named, typed arguments
  , flowBody :: [Statement]       -- The ritual body
  } deriving (Show, Eq)

-- | An argument like user: Identity
data Argument = Argument
  { argName :: String
  , argType :: String
  } deriving (Show, Eq)

-- | Statements inside a flow block
data Statement
  = Source String Expr                  -- source x = ...
  | Delta String Expr                   -- delta x = ...
  | Branch Expr [Statement] [Statement]-- branch cond { ... } else { ... }
  | Mouth Expr                          -- mouth "..."
  | Echo Expr                           -- echo x
  | Pillar String Expr  -- pillar NAME = ...

  deriving (Show, Eq)

-- | Expression types used in RVRS
data Expr
  = Var String                          -- x
  | StrLit String                       -- "hello"
  | BoolLit Bool                        -- true, false
  | Equals Expr Expr                    -- a == b
  | Call String [Expr]                  -- check_trust(user)
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | NumLit Int

  deriving (Show, Eq)
