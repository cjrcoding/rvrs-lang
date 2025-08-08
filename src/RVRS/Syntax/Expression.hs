module RVRS.Syntax.Expression where

import Ya

import RVRS.Syntax.Primitive
import RVRS.Syntax.Operation
import RVRS.Syntax.Identifier

type Expression = Operand `S'T'I'TT'I` Operator `S'T'I'TT'I` Calling

pattern Operand x = T'TT'I'TTT'I (This (T'TT'I'TTT'I (This x))) :: Expression e
pattern Operator x = T'TT'I'TTT'I (This (T'TT'I'TTT'I (That x))) :: Expression e
pattern Calling x = T'TT'I'TTT'I (That (T'TT'I'TTT'I x)) :: Expression e

type Calling = Instead Name `P'T'I'TT'I` List
