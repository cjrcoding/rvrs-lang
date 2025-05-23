module RVRS.Codegen (generateAiken, prettyPrintFlow) where

import RVRS.AST
import RVRS.Parser.Type (RVRSType(..))

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
  Delta var _ expr  -> ["let " ++ var ++ " = " ++ genExpr expr]
  Mouth expr        -> ["trace " ++ show (genExpr expr)]
  Echo expr         -> ["return " ++ genExpr expr]
  Branch cond tBranch fBranch ->
    ["if " ++ genExpr cond ++ " {"] ++
    indent (concatMap genStmt tBranch) ++
    ["} else {"] ++
    indent (concatMap genStmt fBranch) ++
    ["}"]
  Return expr       -> ["return " ++ genExpr expr]
  Whisper expr      -> ["let _ = " ++ genExpr expr]
  Call name args    -> ["let _ = " ++ name ++ "(" ++ commaSep (map genExpr args) ++ ")"]
  Assert expr       -> ["assert(" ++ genExpr expr ++ ")"]
  Pillar name expr  -> ["const " ++ name ++ " = " ++ genExpr expr]

-- | Convert an expression into Aiken-compatible syntax
genExpr :: Expr -> String
genExpr expr = case expr of
  Var x         -> x
  StrLit s      -> show s
  BoolLit True  -> "true"
  BoolLit False -> "false"
  Equals a b    -> genExpr a ++ " == " ++ genExpr b
  GreaterThan a b -> genExpr a ++ " > " ++ genExpr b
  LessThan a b    -> genExpr a ++ " < " ++ genExpr b
  Add a b       -> genExpr a ++ " + " ++ genExpr b
  Sub a b       -> genExpr a ++ " - " ++ genExpr b
  Mul a b       -> genExpr a ++ " * " ++ genExpr b
  Div a b       -> genExpr a ++ " / " ++ genExpr b
  NumLit n      -> show n
  Not e         -> "!" ++ genExpr e
  And a b       -> genExpr a ++ " && " ++ genExpr b
  Or a b        -> genExpr a ++ " || " ++ genExpr b
  CallExpr name args -> name ++ "(" ++ commaSep (map genExpr args) ++ ")"
  Neg e         -> "-" ++ genExpr e

-- | Render a function argument with its type
renderArg :: Argument -> String
renderArg (Argument name typ) = name ++ ": " ++ renderType typ

-- | Convert internal type to user-friendly name
renderType :: RVRSType -> String
renderType TypeNum  = "Num"
renderType TypeStr  = "Str"
renderType TypeBool = "Bool"
renderType TypeAny  = "Any"

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
