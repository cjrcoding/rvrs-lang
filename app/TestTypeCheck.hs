module Main where

import Prelude
import Test.HUnit
import qualified Data.Map as Map

import Ya (Recursive(..), pattern Unit, pattern Ok, pattern Error, by, yi, ho, ho'ho, ha, hv, lu)
import Ya.Instances ()

import RVRS.AST
import RVRS.Checker

-- Define a test environment with some known vars
testEnv :: Map.Map String Typed
testEnv = Map.fromList
  [ ("x", Double Unit)
  , ("y", Bool Unit)
  , ("msg", String Unit)
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
add x y = x `lu` y `lu` Arithmetic `ha` Add `hv` Unit `yi` Binary `ho` Operator `ho` Recursive

equals :: Recursive Expression -> Recursive Expression -> Recursive Expression
equals x y = x `lu` y `lu` Comparison `ha` Equals `hv` Unit `yi` Binary `ho` Operator `ho` Recursive

notExpr :: Recursive Expression -> Recursive Expression
notExpr = (`lu` Complement Unit) `ho` Unary `ho` Operator `ho` Recursive

-- Define the actual tests
tests :: Test
tests = TestList
  [ "Num literal" ~: expression testEnv (num 42) ~?= Ok (Double Unit)
  , "Bool literal" ~: expression testEnv (bool `hv` True) ~?= Ok (Bool Unit)
  , "Str literal" ~: expression testEnv (str "hello") ~?= Ok (String Unit)

  , "Known variable" ~: expression testEnv (var "x") ~?= Ok (Double Unit)
  , "Unknown variable" ~: expression testEnv (var "z") ~?= Error (Unknown "z")

  , "Valid addition" ~: expression testEnv (add (num 5) (num 7)) ~?= Ok (Double Unit)

  , "Invalid addition" ~: TestCase $
      case expression testEnv (add (num 5) (bool `hv` True)) of
        Error (Mismatched _) -> return ()
        _ -> assertFailure "Expected Mismatched"

  , "Equals matching types" ~: expression testEnv (equals (num 1) (num 1)) ~?= Ok (Bool Unit)

  , "Equals mismatched types" ~: TestCase $
      case expression testEnv (equals (str "hi") (bool `hv` False)) of
        Error (Mismatched _) -> return ()
        _ -> assertFailure "Expected Mismatched"

  , "Not on Bool" ~: expression testEnv (notExpr (bool `hv` False)) ~?= Ok (Bool Unit)

  , "Not on Num" ~: TestCase $
      case expression testEnv (notExpr (num 0)) of
        Error (Mismatched _) -> return ()
        _ -> assertFailure "Expected Mismatched"
  ]

main :: IO ()
main = do
  putStrLn "[TEST] Running expression typecheck tests..."
  _ <- runTestTT tests
  return ()
