module Main where

import Test.HUnit
import qualified Data.Map as Map

import Ya (Recursive(..), pattern Unit)

import RVRS.Typecheck.Check (typeOfExpr)
import RVRS.Typecheck.Types
import RVRS.AST

-- Define a test environment with some known vars
testEnv :: TypeEnv
testEnv = Map.fromList
  [ ("x", Double Unit)
  , ("y", Bool Unit)
  , ("msg", String Unit)
  ]

-- Helper to build expressions
num :: Double -> Recursive Expression
num = Recursive . Lit . Double

bool :: Bool -> Recursive Expression
bool = Recursive . Lit . Bool

str :: String -> Recursive Expression
str = Recursive . Lit . String

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
  [ "Num literal" ~: typeOfExpr testEnv (num 42) ~?= Right (Double Unit)
  , "Bool literal" ~: typeOfExpr testEnv (bool True) ~?= Right (Bool Unit)
  , "Str literal" ~: typeOfExpr testEnv (str "hello") ~?= Right (String Unit)

  , "Known variable" ~: typeOfExpr testEnv (var "x") ~?= Right (Double Unit)
  , "Unknown variable" ~: typeOfExpr testEnv (var "z") ~?= Left (UnknownVariable "z")

  , "Valid addition" ~: typeOfExpr testEnv (add (num 5) (num 7)) ~?= Right (Double Unit)

  , "Invalid addition" ~: TestCase $
      case typeOfExpr testEnv (add (num 5) (bool True)) of
        Left (TypeMismatch _ _) -> return ()
        _ -> assertFailure "Expected TypeMismatch"

  , "Equals matching types" ~: typeOfExpr testEnv (equals (num 1) (num 1)) ~?= Right (Bool Unit)

  , "Equals mismatched types" ~: TestCase $
      case typeOfExpr testEnv (equals (str "hi") (bool False)) of
        Left (TypeMismatch _ _) -> return ()
        _ -> assertFailure "Expected TypeMismatch"

  , "Not on Bool" ~: typeOfExpr testEnv (notExpr (bool False)) ~?= Right (Bool Unit)

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
