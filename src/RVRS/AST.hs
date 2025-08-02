module RVRS.AST where

import Ya (T'TT'I'TTT'I (..), type P'T'I'TT'I, type S'T'I'TT'I, Tagged (Tag), type (#), type Twice, type Instead, type T'I' (..), type T, type AR, P, S, Object (..), Recursive (..), type Only, type Both, type Unit, type Nonempty, type List, pattern Instead)

import Ya.ASCII
import Ya.Literal
import Ya.Instances

type Name = Nonempty List ASCII

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
  | Branch (Recursive Expression) (Nonempty List e) (Nonempty List e) -- branch cond { ... } else { ... }
  | Mouth (Recursive Expression)                          -- mouth "..."
  | Whisper (Recursive Expression)
  | Echo (Recursive Expression)                           -- echo x
  | Pillar String (Recursive Expression)  -- pillar NAME = ...
  | Return (Recursive Expression)
  | Call String (Nonempty List `T` Recursive Expression)
  | Assert (Recursive Expression)
  -- deriving (Show, Eq)

type Expression = Operand `S'T'I'TT'I` Operator -- `S'T'I'TT'I` Calling

pattern Operand x = T'TT'I'TTT'I (This x) :: Expression e
pattern Operator x = T'TT'I'TTT'I (That x) :: Expression e
-- pattern Calling x = T'TT'I'TTT'I (That x) :: Expression e

type Operand = Instead Value `S'T'I'TT'I` Instead String

pattern Literal x = T'TT'I'TTT'I (This (Instead x)) :: Operand e
pattern Variable x = T'TT'I'TTT'I (That (Instead x)) :: Operand e

-- type Calling = Instead String `P'T'I'TT'I` Nonempty List

type Operator = Operation Unary Only `S'T'I'TT'I` Operation Dyadic Twice

pattern Unary x = T'TT'I'TTT'I (This x) :: Operator e
pattern Dyadic x = T'TT'I'TTT'I (That x) :: Operator e

type Operation kind quantity = Instead kind `P'T'I'TT'I` quantity

pattern Operation op args = T'TT'I'TTT'I (These (Instead op) args) :: Operation kind quantity e

type Unary = Unit `S` Unit

pattern Negation x = This x
pattern Complement x = That x

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

type Primitive string double bool
 = string `S` double `S` bool

pattern String x = This (This x) :: Primitive string double bool
pattern Double x = This (That x) :: Primitive string double bool
pattern Bool x = That x :: Primitive string double bool

type Value = Primitive String Double Bool

type Typed = Primitive Unit Unit Unit
