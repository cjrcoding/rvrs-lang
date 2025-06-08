{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module RVRS.AST where

import RVRS.Parser.Type (RVRSType(..))


-- | Represents a named flow of ritual logic
data Flow = Flow
  { flowName :: String           -- ^ The flow's name (e.g. "main", "add")
  , flowArgs :: [Argument]       -- ^ Named ritual arguments, optionally typed
  , flowBody :: [Statement]      -- ^ The ritual body (statements)
  } deriving (Show, Eq)

-- | A named argument to a flow, e.g., `x: Number`
data Argument = Argument
  { argName :: String            -- ^ The argument name
  , argType :: String            -- ^ Placeholder for the type (optional for now)
  } deriving (Show, Eq)


-- | Statements inside a flow block
data Statement
  = Source String (Maybe RVRSType) (Recursive Expr)                 -- source x = ...
  | Delta String (Maybe RVRSType) (Recursive Expr)
  | Branch (Recursive Expr) [Statement] [Statement]-- branch cond { ... } else { ... }
  | Mouth (Recursive Expr)                          -- mouth "..."
  | Whisper (Recursive Expr)
  | Echo (Recursive Expr)                           -- echo x
  | Pillar String (Recursive Expr)  -- pillar NAME = ...
  | Return (Recursive Expr)
  | Call String [Recursive Expr]
  | Assert (Recursive Expr)

  deriving (Show, Eq)

data Expr e
  = Var String
  | StrLit String
  | BoolLit Bool
  | Equals e e
  | GreaterThan e e
  | LessThan e e
  | Add e e
  | Sub e e
  | Mul e e
  | Div e e
  | NumLit Double
  | Not e
  | And e e
  | Or e e
  | CallExpr String [e]
  | Neg e
  deriving (Show, Eq)

newtype Recursive f = Recursive { unfix :: f (Recursive f) }

deriving instance (Eq (f (Recursive f))) => Eq (Recursive f)
deriving instance (Show (f (Recursive f))) => Show (Recursive f)
