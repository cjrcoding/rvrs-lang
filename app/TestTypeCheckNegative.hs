module Main where

import Test.HUnit
import qualified Data.Map as Map

import RVRS.Typecheck.Check (typeOfExpr)
import RVRS.Typecheck.Types
import RVRS.AST
import Ya (Recursive(..))

-- Simple type environment
testEnv :: TypeEnv
testEnv = Map.fromList
  [ ("x", TNum)
  , ("y", TBool)
  , ("msg", TStr)
  ]

-- Helpers
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

eq :: Recursive Expression -> Recursive Expression -> Recursive Expression
eq a b = Recursive (Equals a b)

-- Negative test cases (expected to fail)
testBadAddBoolNum :: Test
testBadAddBoolNum = TestCase $
  case typeOfExpr testEnv (add (bool True) (num 1)) of
    Left _ -> return ()  -- ✅ expected failure
    Right t -> assertFailure $ "Unexpected success: got " ++ show t

testBadEqNumStr :: Test
testBadEqNumStr = TestCase $
  case typeOfExpr testEnv (eq (num 5) (str "five")) of
    Left _ -> return ()  -- ✅ expected failure
    Right t -> assertFailure $ "Unexpected success: got " ++ show t

testBadVarUnbound :: Test
testBadVarUnbound = TestCase $
  case typeOfExpr testEnv (var "z") of
    Left _ -> return ()  -- ✅ expected failure
    Right t -> assertFailure $ "Unexpected success: got " ++ show t

testBadNestedAdd :: Test
testBadNestedAdd = TestCase $
  case typeOfExpr testEnv (add (add (bool True) (num 2)) (num 1)) of
    Left _ -> return ()
    Right t -> assertFailure $ "Unexpected success: got " ++ show t

-- Main runner
main :: IO ()
main = do
  putStrLn "🔍 Running negative typeOfExpr tests..."
  _ <- runTestTT $ TestList
    [ testBadAddBoolNum
    , testBadEqNumStr
    , testBadVarUnbound
    , testBadNestedAdd
    ]
  return ()
