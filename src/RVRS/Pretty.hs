module RVRS.Pretty (prettyExpr) where

import Prelude hiding (Bool (..))

import Ya (Recursive (..), is, ho, hu, li, la, type Boolean, pattern False, pattern True)

import RVRS.AST

prettyExpr :: Recursive Expression -> String
prettyExpr expr = case expr of
  Recursive (Variable name) -> name
  Recursive (Operator (Binary (Add e1 e2))) -> "(" ++ prettyExpr e1 ++ " + " ++ prettyExpr e2 ++ ")"
  Recursive (Operator (Binary (Sub e1 e2))) -> "(" ++ prettyExpr e1 ++ " - " ++ prettyExpr e2 ++ ")"
  Recursive (Operator (Binary (Mul e1 e2))) -> "(" ++ prettyExpr e1 ++ " * " ++ prettyExpr e2 ++ ")"
  Recursive (Operator (Binary (Div e1 e2))) -> "(" ++ prettyExpr e1 ++ " / " ++ prettyExpr e2 ++ ")"
  Recursive (Operator (Binary (Equals e1 e2))) -> "(" ++ prettyExpr e1 ++ " == " ++ prettyExpr e2 ++ ")"
  Recursive (Operator (Binary (And e1 e2))) -> "(" ++ prettyExpr e1 ++ " and " ++ prettyExpr e2 ++ ")"
  Recursive (Operator (Binary (Or e1 e2))) -> "(" ++ prettyExpr e1 ++ " or " ++ prettyExpr e2 ++ ")"
  Recursive (Operator (Unary (Not e))) -> "(not " ++ prettyExpr e ++ ")"
  Recursive (Operator (Unary (Neg e))) -> "(-" ++ prettyExpr e ++ ")"
  Recursive (Calling name args) -> "call " ++ name ++ "(" ++ unwords (prettyExpr <$> args) ++ ")"
  Recursive (Literal x) -> is @String `ho` show `la` is @Double `ho` show `la` is @Boolean `ho` (False `hu` "false" `la` True `hu` "true") `li` x