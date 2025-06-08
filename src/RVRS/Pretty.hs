module RVRS.Pretty (prettyExpr) where

import RVRS.AST

prettyExpr :: Recursive Expr -> String
prettyExpr expr = case expr of
  Recursive (NumLit n) -> show n
  Recursive (BoolLit True) -> "truth"
  Recursive (BoolLit False) -> "void"
  Recursive (StrLit s) -> show s
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
