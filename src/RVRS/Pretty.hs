module RVRS.Pretty (prettyExpr) where

import Prelude hiding (Bool (..))

import Ya (type AR, Recursive (..), Object (These), is, ho, ho'he, hu, li, la, la__, type Boolean, pattern False, pattern True)

import RVRS.AST

prettyExpr :: Recursive Expression -> String
prettyExpr expr = case expr of
  Recursive (Variable name) -> name
  Recursive (Operator (Binary (These (These x y) op))) -> "(" ++ prettyExpr x ++ bin op ++ prettyExpr y ++ ")"
  Recursive (Operator (Unary (Not e))) -> "(not " ++ prettyExpr e ++ ")"
  Recursive (Operator (Unary (Neg e))) -> "(-" ++ prettyExpr e ++ ")"
  Recursive (Calling name args) -> "call " ++ name ++ "(" ++ unwords (prettyExpr <$> args) ++ ")"
  Recursive (Literal x) -> is `ho'he` show @String `la` is `ho'he` show @Double `la` is `ho'he` (False `hu` "false" `la` True `hu` "true") `li` x

bin :: Dyadic `AR` String
bin = Add `hu` " + " `la` Sub `hu` " - " `la` Mul `hu` " * " `la` Div `hu` " / "
 `la__` Greater `hu` " > " `la` Equals `hu` " == " `la` Less `hu` " < "
 `la__` And `hu` " and " `la` Or `hu` " or "

