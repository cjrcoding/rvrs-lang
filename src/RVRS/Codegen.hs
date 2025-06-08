module RVRS.Codegen (generateAiken, prettyPrintFlow) where

import RVRS.AST

-- | Convert an entire flow into Aiken-style code
generateAiken :: Flow -> String
generateAiken (Flow name args body) =
  unlines $
    ["fn " ++ name ++ "(" ++ commaSep (map renderArg args) ++ ") -> String {"] ++
    map ("  " ++) (concatMap genStmt body) ++
    ["}"]

-- | Convert a statement into one or more Aiken lines
genStmt :: Statement -> [String]
genStmt stmt = case stmt of
  Source var _ expr -> ["let " ++ var ++ " = " ++ genExpr expr]
  Delta var _ expr -> ["let " ++ var ++ " = " ++ genExpr expr]
  Mouth expr      -> ["trace " ++ show (genExpr expr)]
  Echo expr       -> ["return " ++ genExpr expr]
  Branch cond tBranch fBranch ->
    ["if " ++ genExpr cond ++ " {"] ++
    indent (concatMap genStmt tBranch) ++
    ["} else {"] ++
    indent (concatMap genStmt fBranch) ++
    ["}"]

-- | Convert an expression into Aiken-compatible syntax
genExpr :: Recursive Expression -> String
genExpr expr = case expr of
  Recursive (Var x)         -> x
  Recursive (StrLit s)      -> show s
  Recursive (BoolLit True)  -> "true"
  Recursive (BoolLit False) -> "false"
  Recursive (Equals a b)    -> genExpr a ++ " == " ++ genExpr b


-- | Render a function argument
renderArg :: Argument -> String
renderArg (Argument name typ) = name ++ ": " ++ typ

-- | Helpers
commaSep :: [String] -> String
commaSep []     = ""
commaSep [x]    = x
commaSep (x:xs) = x ++ ", " ++ commaSep xs

indent :: [String] -> [String]
indent = map ("  " ++)

-- | Pretty-print the Flow structure (used for debugging)
prettyPrintFlow :: Flow -> String
prettyPrintFlow (Flow name args body) =
  "Flow\n  name: " ++ name ++
  "\n  args: " ++ show args ++
  "\n  body:\n    " ++ unlines (map show body)
