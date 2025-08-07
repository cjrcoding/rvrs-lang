module RVRS.Codegen (generateAiken, prettyPrintFlow) where

import Prelude
import Data.Bool (bool)
import GHC.IsList (fromList, toList)

import Ya (Object (..), Recursive (..), type T'I' (..), type AR__, type P, pattern Both, is, unwrap, yo, ho, ho'he, hu, la, li)
import Ya.Literal ()

import RVRS.AST

-- | Convert an entire flow into Aiken-style code
generateAiken :: Flow `P` String `AR__` String
generateAiken (These (These args body) name) =
  unlines $
    ["fn " ++ name ++ "(" ++ "..." ++ ") -> String {"] ++
    -- ["fn " ++ name ++ "(" ++ commaSep (toList $ args `yo` renderArg) ++ ") -> String {"] ++
    map ("  " ++) (concatMap genStmt (toList body)) ++
    ["}"]

-- | Convert a statement into one or more Aiken lines
genStmt :: Recursive Statement -> [String]
genStmt stmt = case unwrap stmt of
  -- Source var _ expr -> ["let " ++ var ++ " = " ++ genExpr expr]
  -- Delta var _ expr -> ["let " ++ var ++ " = " ++ genExpr expr]
  Mouth expr -> ["trace " ++ show (genExpr expr)]
  Echo expr -> ["return " ++ genExpr expr]
  Branch cond tBranch fBranch ->
    ["if " ++ genExpr cond ++ " {"] ++
    indent (concatMap genStmt $ toList tBranch) ++
    ["} else {"] ++
    indent (concatMap genStmt $ toList fBranch) ++
    ["}"]

-- | Convert an expression into Aiken-compatible syntax
genExpr :: Recursive Expression -> String
genExpr expr = case unwrap expr of
  -- Operand (Variable x) -> x
  Operand (Literal x) -> is `ho` show @String `la` is `ho` show @Double `la` is `ho` bool "false" "true" `li` x
  -- Operator (Dyadic (These (Both (These x y)) (T'I' (Comparison (Equals _))))) -> genExpr x ++ " == " ++ genExpr y

-- | Render a function argument
-- renderArg :: Argument -> String
-- renderArg (Argument name typ) = name ++ ": " ++ typ

-- | Helpers
commaSep :: [String] -> String
commaSep []     = ""
commaSep [x]    = x
commaSep (x:xs) = x ++ ", " ++ commaSep xs

indent :: [String] -> [String]
indent = map ("  " ++)

-- | Pretty-print the Flow structure (used for debugging)
prettyPrintFlow :: Flow `P` Name `AR__` String
prettyPrintFlow (These (These body args) name) =
  "Flow\n  name: " ++ -- ++ name ++
  "\n  args: " ++ -- show args ++
  "\n  body:\n    " -- ++ unlines (toList $ body `yo` show)
