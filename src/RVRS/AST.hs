module RVRS.AST where

import Ya (P, S, Object (This, That), Recursive (..), type Unit)
import Ya.Instances ()

-- | Represents a named flow of ritual logic
data Flow = Flow
  { flowName :: String                      -- ^ The flow's name (e.g. "main", "add")
  , flowArgs :: [Argument]                  -- ^ Named ritual arguments, optionally typed
  , flowBody :: [Recursive Statement]       -- ^ The ritual body (statements)
  } deriving (Show, Eq)

-- | A named argument to a flow, e.g., `x: Number`
data Argument = Argument
  { argName :: String                       -- ^ The argument name
  , argType :: String                       -- ^ Placeholder for the type (optional for now)
  } deriving (Show, Eq)

-- | Statements inside a flow block
data Statement e
  = Source String (Maybe Typed) (Recursive Expression)          -- source x = ...
  | Delta String (Maybe Typed) (Recursive Expression)
  | Branch (Recursive Expression) [e] [e]                        -- branch cond { ... } else { ... }
  | Mouth (Recursive Expression)                                 -- mouth "..."
  | Whisper (Recursive Expression)
  | Echo (Recursive Expression)                                  -- echo x
  | Pillar String (Recursive Expression)                         -- pillar NAME = ...
  | Return (Recursive Expression)
  | Call String [Recursive Expression]
  | Assert (Recursive Expression)
  deriving (Show, Eq)

data Expression e
  = Variable String
  | Operator (Operation e)
  | Calling String [e]
  | Literal Value
  deriving (Show, Eq)

-- | Generic three-branch primitive structure
type Primitive string double bool = string `S` double `S` bool

-- | Literal patterns
pattern String x = This (This x) :: Primitive string double bool
pattern Double x = This (That x) :: Primitive string double bool
pattern Bool x   = That x        :: Primitive string double bool

-- | Runtime values (string, double, or bool)
type Value = Primitive String Double Bool

-- | Types as units of kind (typed at compile time only)
type Typed = Primitive Unit Unit Unit

-- | Unary and binary operations
data Unary e
  = Neg e
  | Not e
  deriving (Show, Eq)

data Binary e
  = Equals e e
  | Greater e e
  | Less e e
  | Add e e
  | Sub e e
  | Mul e e
  | Div e e
  | And e e
  | Or e e
  deriving (Show, Eq)

-- | Tagged operation sum type
pattern Unary x = This x :: Operation e
pattern Binary x = That x :: Operation e
type Operation e = Unary e `S` Binary e

-- | Intermediate representation of a flow
data FlowIR = FlowIR
  { flowNameIR :: String
  , flowArgsIR :: [String]
  , flowBodyIR :: [Recursive Statement]
  } deriving (Show, Eq)

-- | Enable equality and string display for types
-- deriving instance Eq Typed
-- deriving instance Show Typed
