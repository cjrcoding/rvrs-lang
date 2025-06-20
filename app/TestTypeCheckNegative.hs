module Main where

import Test.HUnit
import qualified Data.Map as Map

import Ya (Recursive(..), pattern Unit, pattern Error, pattern Valid, ho, ho'ho)
import Ya.Instances ()

import RVRS.AST
import RVRS.Checker

-- Simple type environment
testEnv :: Map.Map String Typed
testEnv = Map.fromList
  [ ("x", Double Unit)
  , ("y", Bool Unit)
  , ("msg", String Unit)
  ]

-- Helpers using Ya visual composition
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

eq :: Recursive Expression -> Recursive Expression -> Recursive Expression
eq = Equals `ho'ho` Binary `ho'ho` Operator `ho'ho` Recursive

-- Negative test cases (expected to fail)
testBadAddBoolNum :: Test
testBadAddBoolNum = TestCase $
  case expression testEnv (add (bool True) (num 1)) of
    Error _ -> return ()
    Valid t -> assertFailure $ "Unexpected success: got " ++ show t

testBadEqNumStr :: Test
testBadEqNumStr = TestCase $
  case expression testEnv (eq (num 5) (str "five")) of
    Error _ -> return ()
    Valid t -> assertFailure $ "Unexpected success: got " ++ show t

testBadVarUnbound :: Test
testBadVarUnbound = TestCase $
  case expression testEnv (var "z") of
    Error _ -> return ()
    Valid t -> assertFailure $ "Unexpected success: got " ++ show t

testBadNestedAdd :: Test
testBadNestedAdd = TestCase $
  case expression testEnv (add (add (bool True) (num 2)) (num 1)) of
    Error _ -> return ()
    Valid t -> assertFailure $ "Unexpected success: got " ++ show t

-- Main runner
main :: IO ()
main = do
  putStrLn "[TEST] Running negative expression typecheck tests..."
  _ <- runTestTT $ TestList
    [ testBadAddBoolNum
    , testBadEqNumStr
    , testBadVarUnbound
    , testBadNestedAdd
    ]
  return ()
