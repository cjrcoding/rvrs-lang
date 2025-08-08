module RVRS.Syntax.Operation where

import Ya

import RVRS.Syntax.Primitive
import RVRS.Syntax.Identifier

type Operand = Instead Value `S'T'I'TT'I` Instead Name

pattern Literal x = T'TT'I'TTT'I (This (Instead x)) :: Operand e
pattern Variable x = T'TT'I'TTT'I (That (Instead x)) :: Operand e

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
