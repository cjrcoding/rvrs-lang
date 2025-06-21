module RVRS.Codegen (generateAiken, prettyPrintFlow) where

import Prelude hiding (Bool (..))

import Ya (Object (..), Recursive (..), is, unwrap, ho, ho'he, hu, la, li, type Boolean, pattern False, pattern True)
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
  Variable x -> x
  Literal x -> is `ho'he` show @String `la` is `ho'he` show @Double `la` is `ho'he` genBooleanExpr `li` x
  Operator (Binary (Equals a b)) -> genExpr a ++ " == " ++ genExpr b

genBooleanExpr = False `hu` "false" `la` True `hu` "true"

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
