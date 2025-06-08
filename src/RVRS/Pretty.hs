module RVRS.Pretty (prettyExpr) where

import RVRS.AST

prettyExpr :: Recursive Expr -> String
prettyExpr expr = case expr of
  Recursive (NumLit n) -> show n
  Recursive (BoolLit True) -> "truth"
  Recursive (BoolLit False) -> "void"
  Recursive (StrLit s) -> show s
  Recursive (Var name) -> name

  -- Recursive (Add e1 e2) -> "(" ++ prettyExpr (Recursive e1) ++ " + " ++ prettyExpr (Recursive e2) ++ ")"
  -- Recursive (Sub e1 e2) -> "(" ++ prettyExpr (Recursive e1) ++ " - " ++ prettyExpr (Recursive e2) ++ ")"
  -- Recursive (Mul e1 e2) -> "(" ++ prettyExpr (Recursive e1) ++ " * " ++ prettyExpr (Recursive e2) ++ ")"
  -- Recursive (Div e1 e2) -> "(" ++ prettyExpr (Recursive e1) ++ " / " ++ prettyExpr (Recursive e2) ++ ")"

  -- Recursive (Equals e1 e2) -> "(" ++ prettyExpr (Recursive e1) ++ " == " ++ prettyExpr (Recursive e2) ++ ")"
  -- Recursive (And e1 e2) -> "(" ++ prettyExpr (Recursive e1) ++ " and " ++ prettyExpr (Recursive e2) ++ ")"
  -- Recursive (Or e1 e2) -> "(" ++ prettyExpr (Recursive e1) ++ " or " ++ prettyExpr (Recursive e2) ++ ")"
  -- Recursive (Not e) -> "(not " ++ prettyExpr (Recursive e) ++ ")"
  -- Recursive (Neg e) -> "(-" ++ prettyExpr (Recursive e) ++ ")"

  -- Recursive (CallExpr name args) -> "call " ++ name ++ "(" ++ unwords (prettyExpr . Recursive <$> args) ++ ")"
