module Main where

import Prelude
import Test.HUnit
import qualified Data.Map as Map

import Ya (Recursive(..), pattern Unit, pattern Ok, pattern Only, pattern Both, pattern Error, is, this, by, yi, yo, ho, ho'he, ho'ho, ha, hv, he'he'hv, lu)
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
num = Double `ho` Literal `ho` Operand `ho` Recursive

bool :: Bool -> Recursive Expression
bool = Bool `ho` Literal `ho` Operand `ho` Recursive

str :: String -> Recursive Expression
str = String `ho` Literal `ho` Operand `ho` Recursive

var :: String -> Recursive Expression
var = Variable `ho` Operand `ho` Recursive

-- add :: Recursive Expression -> Recursive Expression -> Recursive Expression
-- add x y = x `lu` y `lu` Arithmetic `ha` Add `hv` Unit `yi` Dyadic `ho` Operator `ho` Recursive

-- equals :: Recursive Expression -> Recursive Expression -> Recursive Expression
-- equals x y = Operation `hv` Both (x `lu` y) `hv` (Arithmetic `hv` Equals Unit) `yi` Dyadic `ho` Operator `ho` Recursive

notExpr :: Recursive Expression -> Recursive Expression
notExpr x = Operation `hv` Complement Unit `hv` Only x `yi` Unary `ho` Operator `ho` Recursive

-- type Operator = Operation Only Unary `S'T'I'TT'I` Operation Twice Dyadic

-- pattern Unary x = T'TT'I'TTT'I (This x) :: Operator e
-- pattern Dyadic x = T'TT'I'TTT'I (That x) :: Operator e

-- type Operation quantity kind = quantity `P'T'I'TT'I` Instead kind

-- pattern Operation args op = T'TT'I'TTT'I (These args (Instead op)) :: Operation quantity kind e

-- Define the actual tests
tests :: Test
tests = TestList
  [ "Num literal"
    ~: expression `hv` num 42
       `he'he'hv` testEnv `yo` is `ho'he` this @Typed
    ~?= Ok (Double Unit)

  , "Bool literal"
    ~: expression `hv` bool True
       `he'he'hv` testEnv `yo` is `ho'he` this @Typed
    ~?= Ok (Bool Unit)

  , "String literal"
    ~: expression `hv` str "hello"
       `he'he'hv` testEnv `yo` is `ho'he` this @Typed
    ~?= Ok (String Unit)

  , "Known variable"
    ~: expression `hv` var "x"
       `he'he'hv` testEnv `yo` is `ho'he` this @Typed
    ~?= Ok (Double Unit)

  , "Unknown variable"
    ~: expression `hv` var "x"
       `he'he'hv` testEnv `yo` is `ho'he` this @Typed
    ~?= Error (Unknown "z")

  -- , "Valid addition" ~: expression testEnv (add (num 5) (num 7)) ~?= Ok (Double Unit)

  -- , "Invalid addition" ~: TestCase $
      -- case expression testEnv (add (num 5) (bool `hv` True)) of
        -- Error (Mismatched _) -> return ()
        -- _ -> assertFailure "Expected Mismatched"

  -- , "Equals matching types" ~: expression testEnv (equals (num 1) (num 1)) ~?= Ok (Bool Unit)

  -- , "Equals mismatched types" ~: TestCase $
      -- case expression testEnv (equals (str "hi") (bool `hv` False)) of
        -- Error (Mismatched _) -> return ()
        -- _ -> assertFailure "Expected Mismatched"

  , "Not on Bool"
    ~: expression `ha` notExpr `ha` bool `hv` False
       `he'he'hv` testEnv `yo` is `ho'he` this @Typed
    ~?= Ok (Bool Unit)

  , "Not on Num"
    ~: TestCase $ case expression `ha` notExpr `hv` num 0 `he'he'hv` testEnv of
        Error (Mismatched _) -> return ()
        _ -> assertFailure "Expected Mismatched"
  ]

main :: IO ()
main = do
  putStrLn "[TEST] Running expression typecheck tests..."
  _ <- runTestTT tests
  return ()
