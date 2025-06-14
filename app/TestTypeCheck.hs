module Main where

import Test.HUnit
import qualified Data.Map as Map

import RVRS.Typecheck.Check (typeOfExpr)
import RVRS.Typecheck.Types
import RVRS.AST
import Ya (Recursive(..), ha, ho, ho'ho)


-- Define a test environment with some known vars
testEnv :: TypeEnv
testEnv = Map.fromList
  [ ("x", TNum)
  , ("y", TBool)
  , ("msg", TStr)
  ]

-- Helper to build expressions
num :: Double -> Recursive Expression
num = Double `ho` Literal `ho` Recursive

bool :: Bool -> Recursive Expression
bool = Bool `ho` Literal `ho` Recursive

str :: String -> Recursive Expression
str = String `ho` Literal `ho` Recursive

var :: String -> Recursive Expression
var = Variable `ho` Recursive

add :: Recursive Expression -> Recursive Expression -> Recursive Expression
add = Add `ho'ho` Binary `ho'ho` Operator `ho'ho` Recursive

equals :: Recursive Expression -> Recursive Expression -> Recursive Expression
equals = Equals `ho'ho` Binary `ho'ho` Operator `ho'ho` Recursive

notExpr :: Recursive Expression -> Recursive Expression
notExpr = Not `ho` Unary `ho` Operator `ho` Recursive

-- Define the actual tests
tests :: Test
tests = TestList
  [ "Num literal" ~: typeOfExpr testEnv (num 42) ~?= Right TNum
  , "Bool literal" ~: typeOfExpr testEnv (bool True) ~?= Right TBool
  , "Str literal" ~: typeOfExpr testEnv (str "hello") ~?= Right TStr

  , "Known variable" ~: typeOfExpr testEnv (var "x") ~?= Right TNum
  , "Unknown variable" ~: typeOfExpr testEnv (var "z") ~?= Left (UnknownVariable "z")

  , "Valid addition" ~: typeOfExpr testEnv (add (num 5) (num 7)) ~?= Right TNum

  , "Invalid addition" ~: TestCase $
      case typeOfExpr testEnv (add (num 5) (bool True)) of
        Left (TypeMismatch _ _) -> return ()
        _ -> assertFailure "Expected TypeMismatch"

  , "Equals matching types" ~: typeOfExpr testEnv (equals (num 1) (num 1)) ~?= Right TBool

  , "Equals mismatched types" ~: TestCase $
      case typeOfExpr testEnv (equals (str "hi") (bool False)) of
        Left (TypeMismatch _ _) -> return ()
        _ -> assertFailure "Expected TypeMismatch"

  , "Not on Bool" ~: typeOfExpr testEnv (notExpr (bool False)) ~?= Right TBool

  , "Not on Num" ~: TestCase $
      case typeOfExpr testEnv (notExpr (num 0)) of
        Left (TypeMismatch _ _) -> return ()
        _ -> assertFailure "Expected TypeMismatch"
  ]

main :: IO ()
main = do
  putStrLn "🔍 Running typeOfExpr tests..."
  _ <- runTestTT tests
  return ()