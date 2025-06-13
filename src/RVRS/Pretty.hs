module RVRS.Pretty (prettyExpr) where

import Data.Bool (bool)
import Ya (Recursive (..), is, ho, li, la)

import RVRS.AST

prettyExpr :: Recursive Expression -> String
prettyExpr expr = case expr of
  Recursive (Lit x) -> is @String `ho` show `la` is @Double `ho` show `la` is @Bool `ho` bool "false" "true" `li` x
  Recursive (Var name) -> name

  Recursive (Add e1 e2) -> "(" ++ prettyExpr e1 ++ " + " ++ prettyExpr e2 ++ ")"
  Recursive (Sub e1 e2) -> "(" ++ prettyExpr e1 ++ " - " ++ prettyExpr e2 ++ ")"
  Recursive (Mul e1 e2) -> "(" ++ prettyExpr e1 ++ " * " ++ prettyExpr e2 ++ ")"
  Recursive (Div e1 e2) -> "(" ++ prettyExpr e1 ++ " / " ++ prettyExpr e2 ++ ")"

  Recursive (Equals e1 e2) -> "(" ++ prettyExpr e1 ++ " == " ++ prettyExpr e2 ++ ")"
  Recursive (And e1 e2) -> "(" ++ prettyExpr e1 ++ " and " ++ prettyExpr e2 ++ ")"
  Recursive (Or e1 e2) -> "(" ++ prettyExpr e1 ++ " or " ++ prettyExpr e2 ++ ")"
  Recursive (Not e) -> "(not " ++ prettyExpr e ++ ")"
  Recursive (Neg e) -> "(-" ++ prettyExpr e ++ ")"

  Recursive (CallExpr name args) -> "call " ++ name ++ "(" ++ unwords (prettyExpr <$> args) ++ ")"
