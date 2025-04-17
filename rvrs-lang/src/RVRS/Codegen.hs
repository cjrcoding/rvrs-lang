module RVRS.Codegen (generateAiken) where

import RVRS.AST

-- | Convert an entire flow into Aiken-style code
generateAiken :: Flow -> String
generateAiken (Flow name args body) =
  unlines $
    ["fn " ++ name ++ "(" ++ unwords args ++ ") -> String {"] ++
    map ("  " ++) (concatMap genStmt body) ++
    ["}"]

-- | Convert a statement into one or more Aiken lines
genStmt :: Statement -> [String]
genStmt stmt = case stmt of
  Source var expr -> ["let " ++ var ++ " = " ++ genExpr expr]
  Delta var expr  -> ["let " ++ var ++ " = " ++ genExpr expr]
  Mouth expr      -> ["trace " ++ show (genExpr expr)]  -- Wrapped in `show` to add quotes
  Echo expr       -> ["return " ++ genExpr expr]
  Branch cond tBranch fBranch ->
    [ "if " ++ genExpr cond ++ " {" ] ++
    indent (concatMap genStmt tBranch) ++
    ["} else {"] ++
    indent (concatMap genStmt fBranch) ++
    ["}"]

-- | Convert an expression into Aiken-compatible syntax
genExpr :: Expr -> String
genExpr expr = case expr of
  Var x         -> x
  StrLit s      -> show s          -- Adds quotes
  BoolLit True  -> "true"
  BoolLit False -> "false"
  Equals a b    -> genExpr a ++ " == " ++ genExpr b
  Call f args   -> f ++ "(" ++ commaSep (map genExpr args) ++ ")"

-- | Helpers
commaSep :: [String] -> String
commaSep = foldr1 (\a b -> a ++ ", " ++ b)

indent :: [String] -> [String]
indent = map ("  " ++)
