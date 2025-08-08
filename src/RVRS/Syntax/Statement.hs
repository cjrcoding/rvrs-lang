module RVRS.Syntax.Statement where

import RVRS.Syntax.Primitive
import RVRS.Syntax.Operation
import RVRS.Syntax.Identifier
import RVRS.Syntax.Expression

import Ya

-- TODO: open this datatype
data Statement e
  = Source Name (Recursive Expression)
  | Delta Name (Recursive Expression)
  | Branch (Recursive Expression) (Nonempty List e) (Nonempty List e)
  | Mouth (Recursive Expression)
  | Whisper (Recursive Expression)
  | Echo (Recursive Expression)
  | Pillar Name (Recursive Expression)
  | Return (Recursive Expression)
  | Assert (Recursive Expression)
