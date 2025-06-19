module Main where

import Prelude hiding (Bool (..))
import Test.HUnit
import qualified Data.Map as Map

import Ya (Recursive(..), type Boolean, pattern Unit, pattern True, pattern False, pattern Error, pattern Valid)

import RVRS.AST
import RVRS.Checker

-- Simple type environment
testEnv :: Map.Map String Typed
testEnv = Map.fromList
  [ ("x", Double Unit)
  , ("y", Bool Unit)
  , ("msg", String Unit)
  ]

-- Helpers
num :: Double -> Recursive Expression
num = Recursive . Literal . Double

bool :: Boolean -> Recursive Expression
bool = Recursive . Literal . Bool

str :: String -> Recursive Expression
str = Recursive . Literal . String

var :: String -> Recursive Expression
var = Recursive . Variable

add :: Recursive Expression -> Recursive Expression -> Recursive Expression
add a b = Recursive (Operator (Binary (Add a b)))

eq :: Recursive Expression -> Recursive Expression -> Recursive Expression
eq a b = Recursive (Operator (Binary (Equals a b)))

-- Negative test cases (expected to fail)
testBadAddBoolNum :: Test
testBadAddBoolNum = TestCase $
  case expression testEnv (add (bool (True Unit)) (num 1)) of
    Error _ -> return ()  -- ✅ expected failure
    Valid t -> assertFailure $ "Unexpected success: got " ++ show t

testBadEqNumStr :: Test
testBadEqNumStr = TestCase $
  case expression testEnv (eq (num 5) (str "five")) of
    Error _ -> return ()  -- ✅ expected failure
    Valid t -> assertFailure $ "Unexpected success: got " ++ show t

testBadVarUnbound :: Test
testBadVarUnbound = TestCase $
  case expression testEnv (var "z") of
    Error _ -> return ()  -- ✅ expected failure
    Valid t -> assertFailure $ "Unexpected success: got " ++ show t

testBadNestedAdd :: Test
testBadNestedAdd = TestCase $
  case expression testEnv (add (add (bool (True Unit)) (num 2)) (num 1)) of
    Error _ -> return ()
    Valid t -> assertFailure $ "Unexpected success: got " ++ show t

-- Main runner
main :: IO ()
main = do
  putStrLn "🔍 Running negative expression tests..."
  _ <- runTestTT $ TestList
    [ testBadAddBoolNum
    , testBadEqNumStr
    , testBadVarUnbound
    , testBadNestedAdd
    ]
  return ()
