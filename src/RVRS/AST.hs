-- src/RVRS/AST.hs

module RVRS.AST where

-- Represents an entire RVRS flow
data Flow = Flow
  { flowName :: String
  , flowArgs :: [Argument]        -- custom Argument type
  , flowBody :: [Statement]
  } deriving (Show, Eq)

-- Represents a named, typed argument like user: Identity
data Argument = Argument
  { argName :: String
  , argType :: String
  } deriving (Show, Eq)

-- Individual statements inside a flow
data Statement
  = Source String Expr           -- source x = ...
  | Delta String Expr            -- delta x = ...
  | Branch Expr [Statement] [Statement] -- if/else
  | Mouth Expr                   -- mouth "output"
  | Echo Expr                    -- echo x
  deriving (Show, Eq)

-- Expressions used in statements
data Expr
  = Var String                   -- x
  | StrLit String               -- "hello"
  | BoolLit Bool                -- truth / void
  | Equals Expr Expr            -- a == b
  | Call String [Expr]          -- check_trust(user)
  deriving (Show, Eq)
