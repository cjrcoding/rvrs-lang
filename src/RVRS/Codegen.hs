module RVRS.Codegen (generateAiken, prettyPrintFlow) where

import Data.Bool (bool)

import Ya (Recursive (..), is, unwrap, ho, hu, la, li)
import qualified Ya as Y

import RVRS.AST

-- | Convert an entire flow into Aiken-style code
generateAiken :: Flow -> String
generateAiken (Flow name args body) =
  unlines $
    ["fn " ++ name ++ "(" ++ commaSep (map renderArg args) ++ ") -> String {"] ++
    map ("  " ++) (concatMap genStmt body) ++
    ["}"]

-- | Convert a statement into one or more Aiken lines
genStmt :: Recursive Statement -> [String]
genStmt stmt = case unwrap stmt of
  Source var _ expr -> ["let " ++ var ++ " = " ++ genExpr expr]
  Delta var _ expr -> ["let " ++ var ++ " = " ++ genExpr expr]
  Mouth expr -> ["trace " ++ show (genExpr expr)]
  Echo expr -> ["return " ++ genExpr expr]
  Branch cond tBranch fBranch ->
    ["if " ++ genExpr cond ++ " {"] ++
    indent (concatMap genStmt tBranch) ++
    ["} else {"] ++
    indent (concatMap genStmt fBranch) ++
    ["}"]

-- | Convert an expression into Aiken-compatible syntax
genExpr :: Recursive Expression -> String
genExpr expr = case unwrap expr of
  Var x         -> x
  Lit x   -> genLit x
  -- StrLit s      -> show s
  -- BoolLit True  -> "true"
  -- BoolLit False -> "false"
  Equals a b    -> genExpr a ++ " == " ++ genExpr b

genLit = is @String `ho` show `la` is @Double `ho` show `la` is @Bool `ho` bool "false" "true"

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
