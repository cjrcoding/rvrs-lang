module RVRS.AST where

import Ya (S, Object (This, That), Recursive (..))

import RVRS.Parser.Type (RVRSType(..))
import Ya.Instances ()

-- | Represents a named flow of ritual logic
data Flow = Flow
  { flowName :: String           -- ^ The flow's name (e.g. "main", "add")
  , flowArgs :: [Argument]       -- ^ Named ritual arguments, optionally typed
  , flowBody :: [Recursive Statement]      -- ^ The ritual body (statements)
  } deriving (Show, Eq)

-- | A named argument to a flow, e.g., `x: Number`
data Argument = Argument
  { argName :: String            -- ^ The argument name
  , argType :: String            -- ^ Placeholder for the type (optional for now)
  } deriving (Show, Eq)


-- | Statements inside a flow block
data Statement e
  = Source String (Maybe RVRSType) (Recursive Expression)                 -- source x = ...
  | Delta String (Maybe RVRSType) (Recursive Expression)
  | Branch (Recursive Expression) [e] [e]-- branch cond { ... } else { ... }
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
  | Lit Primitive
  | Equals e e
  | GreaterThan e e
  | LessThan e e
  | Add e e
  | Sub e e
  | Mul e e
  | Div e e
  | Not e
  | And e e
  | Or e e
  | CallExpr String [e]
  | Neg e
  deriving (Show, Eq)

type Primitive = String `S` Double `S` Bool

pattern String x = This (This x) :: Primitive
pattern Double x = This (That x) :: Primitive
pattern Bool x = That x :: Primitive

-- | Intermediate representation of a flow
data FlowIR = FlowIR
  { flowNameIR :: String
  , flowArgsIR :: [String]
  , flowBodyIR :: [Recursive Statement]
  } deriving (Show, Eq)
