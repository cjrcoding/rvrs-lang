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
  = Source String (Maybe RVRSType) (Recursive Expression)                 -- source x = ...
  | Delta String (Maybe RVRSType) (Recursive Expression)
  | Branch (Recursive Expression) [Statement] [Statement]-- branch cond { ... } else { ... }
  | Mouth (Recursive Expression)                          -- mouth "..."
  | Whisper (Recursive Expression)
  | Echo (Recursive Expression)                           -- echo x
  | Pillar String (Recursive Expression)  -- pillar NAME = ...
  | Return (Recursive Expression)
  | Call String [Recursive Expression]
  | Assert (Recursive Expression)

  deriving (Show, Eq)

data Expression e
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

-- | Intermediate representation of a flow
data FlowIR = FlowIR
  { flowNameIR :: String
  , flowArgsIR :: [String]
  , flowBodyIR :: [StmtIR]
  } deriving (Show, Eq)

-- | Lowered statements (IR version of AST Statement)
data StmtIR
  = IRDelta String (Recursive Expression) (Maybe RVRSType)
  | IRSource String (Recursive Expression) (Maybe RVRSType)
  | IREcho (Recursive Expression)
  | IRWhisper String (Recursive Expression)
  | IRMouth (Recursive Expression)
  | IRBranch (Recursive Expression) [StmtIR] [StmtIR]
  | IRReturn (Recursive Expression)
  | IRCallStmt String [Recursive Expression]  -- Top-level statement like: call foo(x, y)
  | IRAssert (Recursive Expression)
  deriving (Show, Eq)