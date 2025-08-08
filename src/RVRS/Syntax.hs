{-# LANGUAGE NoImplicitPrelude #-}
module RVRS.Syntax where

import Ya
import Ya.ASCII
import Ya.Literal
import Ya.Instances

import Prelude (Bool, Double, String)

type Name = Nonempty List Letter

-- TODO: add optional type annotation (or not optional?)
type Flow = List Name `P` Nonempty List (Recursive Statement)

data Statement e
  = Source Name (Recursive Expression)                 -- source x = ...
  | Delta Name (Recursive Expression)
  | Branch (Recursive Expression) (Nonempty List e) (Nonempty List e) -- branch cond { ... } else { ... }
  | Mouth (Recursive Expression)                          -- mouth "..."
  | Whisper (Recursive Expression)
  | Echo (Recursive Expression)                           -- echo x
  | Pillar Name (Recursive Expression)  -- pillar NAME = ...
  | Return (Recursive Expression)
  | Assert (Recursive Expression)

type Expression = Operand `S'T'I'TT'I` Operator `S'T'I'TT'I` Calling

pattern Operand x = T'TT'I'TTT'I (This (T'TT'I'TTT'I (This x))) :: Expression e
pattern Operator x = T'TT'I'TTT'I (This (T'TT'I'TTT'I (That x))) :: Expression e
pattern Calling x = T'TT'I'TTT'I (That (T'TT'I'TTT'I x)) :: Expression e

type Operand = Instead Value `S'T'I'TT'I` Instead Name

pattern Literal x = T'TT'I'TTT'I (This (Instead x)) :: Operand e
pattern Variable x = T'TT'I'TTT'I (That (Instead x)) :: Operand e

type Operator = Operation Unary Only `S'T'I'TT'I` Operation Dyadic Twice

pattern Unary x = T'TT'I'TTT'I (This x) :: Operator e
pattern Dyadic x = T'TT'I'TTT'I (That x) :: Operator e

type Calling = Instead Name `P'T'I'TT'I` List

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
