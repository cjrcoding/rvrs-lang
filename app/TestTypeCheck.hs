module Main where

import Test.HUnit
import qualified Data.Map as Map

import RVRS.Typecheck.Check (typeofExpr)
import RVRS.Typecheck.Types
import RVRS.AST
import RVRS.AST (Recursive(..)) 


-- Define a test environment with some known vars
testEnv :: TypeEnv
testEnv = Map.fromList
  [ ("x", TNum)
  , ("y", TBool)
  , ("msg", TStr)
  ]

-- Helper to build expressions
num :: Double -> Recursive Expression
num = Recursive . NumLit

bool :: Bool -> Recursive Expression
bool = Recursive . BoolLit

str :: String -> Recursive Expression
str = Recursive . StrLit

var :: String -> Recursive Expression
var = Recursive . Var

add :: Recursive Expression -> Recursive Expression -> Recursive Expression
add a b = Recursive (Add a b)

equals :: Recursive Expression -> Recursive Expression -> Recursive Expression
equals a b = Recursive (Equals a b)

notExpr :: Recursive Expression -> Recursive Expression
notExpr = Recursive . Not

-- Define the actual tests
tests :: Test
tests = TestList
  [ "Num literal" ~: typeofExpr testEnv (num 42) ~?= Right TNum
  , "Bool literal" ~: typeofExpr testEnv (bool True) ~?= Right TBool
  , "Str literal" ~: typeofExpr testEnv (str "hello") ~?= Right TStr

  , "Known variable" ~: typeofExpr testEnv (var "x") ~?= Right TNum
  , "Unknown variable" ~: typeofExpr testEnv (var "z") ~?= Left (UnknownVariable "z")

  , "Valid addition" ~: typeofExpr testEnv (add (num 5) (num 7)) ~?= Right TNum

  , "Invalid addition" ~: TestCase $
      case typeofExpr testEnv (add (num 5) (bool True)) of
        Left (TypeMismatch _ _) -> return ()
        _ -> assertFailure "Expected TypeMismatch"

  , "Equals matching types" ~: typeofExpr testEnv (equals (num 1) (num 1)) ~?= Right TBool

  , "Equals mismatched types" ~: TestCase $
      case typeofExpr testEnv (equals (str "hi") (bool False)) of
        Left (TypeMismatch _ _) -> return ()
        _ -> assertFailure "Expected TypeMismatch"

  , "Not on Bool" ~: typeofExpr testEnv (notExpr (bool False)) ~?= Right TBool

  , "Not on Num" ~: TestCase $
      case typeofExpr testEnv (notExpr (num 0)) of
        Left (TypeMismatch _ _) -> return ()
        _ -> assertFailure "Expected TypeMismatch"
  ]

main :: IO ()
main = do
  putStrLn "🔍 Running typeOfExpr tests..."
  _ <- runTestTT tests
  return ()
