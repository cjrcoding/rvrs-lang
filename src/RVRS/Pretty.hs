module RVRS.Pretty (prettyExpr) where

import Prelude
import Data.Bool (bool)
import GHC.IsList (toList)

import Ya (type AR, Recursive (..), Object (These), is, ho, hu, li, la, la__)

import RVRS.AST

prettyExpr :: Recursive Expression -> String
prettyExpr expr = case expr of
  -- Recursive (Operand (Variable name)) -> name
  Recursive (Operand (Literal x)) -> is `ho` show @String `la` is `ho` show @Double `la` is `ho` bool "false" "true" `li` x
  -- Recursive (Operator (Dyadic (These (These x y) op))) -> "(" ++ prettyExpr x ++ dyadic op ++ prettyExpr y ++ ")"
  -- Recursive (Operator (Unary (These e op))) -> "(" ++ unary op ++ prettyExpr e ++ ")"
  -- Recursive (Calling name args) -> "call " ++ name ++ "(" ++ unwords (prettyExpr <$> toList args) ++ ")"

dyadic :: Dyadic `AR` String
dyadic = Add `hu` " + " `la` Sub `hu` " - " `la` Mul `hu` " * " `la` Div `hu` " / "
 `la__` Greater `hu` " > " `la` Equals `hu` " == " `la` Less `hu` " < "
 `la__` And `hu` " and " `la` Or `hu` " or "

unary :: Unary `AR` String
unary = Negation `hu` "not " `la` Complement `hu` "-"
