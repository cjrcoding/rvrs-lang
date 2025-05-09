module RVRS.Pretty (prettyExpr) where

import RVRS.AST

prettyExpr :: Expr -> String
prettyExpr expr = case expr of
  NumLit n     -> show n
  BoolLit True -> "truth"
  BoolLit False -> "void"
  StrLit s     -> show s
  Var name     -> name

  Add e1 e2    -> "(" ++ prettyExpr e1 ++ " + " ++ prettyExpr e2 ++ ")"
  Sub e1 e2    -> "(" ++ prettyExpr e1 ++ " - " ++ prettyExpr e2 ++ ")"
  Mul e1 e2    -> "(" ++ prettyExpr e1 ++ " * " ++ prettyExpr e2 ++ ")"
  Div e1 e2    -> "(" ++ prettyExpr e1 ++ " / " ++ prettyExpr e2 ++ ")"

  Equals e1 e2 -> "(" ++ prettyExpr e1 ++ " == " ++ prettyExpr e2 ++ ")"
  And e1 e2    -> "(" ++ prettyExpr e1 ++ " and " ++ prettyExpr e2 ++ ")"
  Or e1 e2     -> "(" ++ prettyExpr e1 ++ " or " ++ prettyExpr e2 ++ ")"
  Not e        -> "(not " ++ prettyExpr e ++ ")"
  Neg e        -> "(-" ++ prettyExpr e ++ ")"

  CallExpr name args -> "call " ++ name ++ "(" ++ unwords (map prettyExpr args) ++ ")"
