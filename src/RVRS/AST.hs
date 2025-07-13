module RVRS.AST where

import Ya (Tagged (Tag), type (#), type T, type AR, P, S, Object (This, That), Recursive (..), type Unit, type Nonempty, type List)

import Ya.Instances ()
import Ya.Literal ()

-- | Represents a flow of ritual logic
type Flow = Nonempty List Argument `P` Nonempty List (Recursive Statement)

-- TODO: is `argType` really necessary here?
-- | A named argument to a flow, e.g., `x: Number`
data Argument = Argument
  { argName :: String            -- ^ The argument name
  , argType :: String            -- ^ Placeholder for the type (optional for now)
  } deriving (Show, Eq)

-- | Statements inside a flow block
data Statement e
  = Source String (Maybe Typed) (Recursive Expression)                 -- source x = ...
  | Delta String (Maybe Typed) (Recursive Expression)
  | Branch (Recursive Expression) (Nonempty List e) (Nonempty List e)-- branch cond { ... } else { ... }
  | Mouth (Recursive Expression)                          -- mouth "..."
  | Whisper (Recursive Expression)
  | Echo (Recursive Expression)                           -- echo x
  | Pillar String (Recursive Expression)  -- pillar NAME = ...
  | Return (Recursive Expression)
  | Call String (Nonempty List `T` Recursive Expression)
  | Assert (Recursive Expression)
  deriving (Show, Eq)

data Expression e
  = Variable String
  | Operator (Operation e)
  | Calling String (Nonempty List e)
  | Literal Value
  deriving (Show, Eq)

type Primitive string double bool
 = (String # string) `S` (Double # double) `S` (Bool # bool)

pattern String x = This (This (Tag @String x)) :: Primitive string double bool
pattern Double x = This (That (Tag @Double x)) :: Primitive string double bool
pattern Bool x = That (Tag @Bool x) :: Primitive string double bool

type Value = Primitive String Double Bool

type Typed = Primitive Unit Unit Unit

data Unary e
  = Neg e
  | Not e
  deriving (Show, Eq)

type Dyadic = Arithmetic `S` Comparison `S` Combinated

pattern Arithmetic x = This (This x)
pattern Comparison x = This (That x)
pattern Combinated x = That x

type Arithmetic = Unit `S` Unit `S` Unit `S` Unit

pattern Add, Sub, Mul, Div :: Unit `AR` Arithmetic
pattern Add x = This (This (This x))
pattern Sub x = This (This (That x))
pattern Mul x = This (That x)
pattern Div x = That x

type Comparison = Unit `S` Unit `S` Unit

pattern Greater, Equals, Less :: Unit `AR` Comparison
pattern Greater x = This (This x)
pattern Equals x = This (That x)
pattern Less x = That x

type Combinated = Unit `S` Unit

pattern And, Or :: Unit `AR` Combinated
pattern And x = This x
pattern Or x = That x

type Operation e = Unary e `S` (e `P` e `P` Dyadic)

pattern Unary x = This x :: Operation e
pattern Binary x = That x :: Operation e
