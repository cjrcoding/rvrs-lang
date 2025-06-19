{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude (String, Double, Bool(..), IO, ($), (>>), putStrLn, return, show, Eq(..))
import Test.HUnit
import qualified Data.Map as Map

import Ya (Recursive(..), ho, ho'ho)
import Ya.Instances ()
import Ya.Program.Patterns (pattern Ok, pattern Error)

import RVRS.AST
import RVRS.Typecheck.Stmt
import RVRS.Typecheck.Types

-- Optional type synonyms for clarity
pattern TNum = Double Unit
pattern TBool = Bool Unit
pattern TStr = String Unit

-- Type environment with some known bindings
testEnv :: Map.Map String Typed
testEnv = Map.fromList
  [ ("x", TNum)
  , ("y", TBool)
  , ("msg", TStr)
  ]

-- Expression helpers (visual composition style)
num :: Double -> Recursive Expression
num = Double `ho` Literal `ho` Recursive

bool :: Bool -> Recursive Expression
bool = Bool `ho` Literal `ho` Recursive

str :: String -> Recursive Expression
str = String `ho` Literal `ho` Recursive

var :: String -> Recursive Expression
var = Variable `ho` Recursive

-- Statement helpers
stmt :: Statement (Recursive Statement) -> Recursive Statement
stmt = Recursive

delta :: String -> Maybe Typed -> Recursive Expression -> Recursive Statement
delta name typ expr = stmt (Delta name typ expr)

source :: String -> Maybe Typed -> Recursive Expression -> Recursive Statement
source name typ expr = stmt (Source name typ expr)

assert :: Recursive Expression -> Recursive Statement
assert = Assert `ho` Recursive

branch :: Recursive Expression -> [Recursive Statement] -> [Recursive Statement] -> Recursive Statement
branch cond tb fb = Branch cond tb fb `ho` Recursive

ret :: Recursive Expression -> Recursive Statement
ret = Return `ho` Recursive

-- Tests
tests :: Test
tests = TestList
  [ "Delta with matching type" ~:
      typeOfStmt testEnv (delta "z" (Just TNum) (num 42))
        ~?= Ok (Map.insert "z" TNum testEnv)

  , "Delta with type mismatch" ~: TestCase $
      case typeOfStmt testEnv (delta "z" (Just TBool) (num 42)) of
        Error (TypeMismatchStmt "z" TBool TNum) -> return ()
        _ -> assertFailure "Expected TypeMismatchStmt"

  , "Source redefinition error" ~: TestCase $
      case typeOfStmt testEnv (source "x" (Just TNum) (num 1)) of
        Error (RedefinedVar "x") -> return ()
        _ -> assertFailure "Expected RedefinedVar"

  , "Valid assert on Bool" ~:
      typeOfStmt testEnv (assert (bool True)) ~?= Ok testEnv

  , "Invalid assert on Num" ~: TestCase $
      case typeOfStmt testEnv (assert (num 99)) of
        Error (BadAssertType TNum) -> return ()
        _ -> assertFailure "Expected BadAssertType"

  , "Branch with Bool condition" ~:
      typeOfStmt testEnv (branch (bool True)
        [delta "z" (Just TNum) (num 1)]
        [delta "z" (Just TNum) (num 2)])
        ~?= Ok testEnv

  , "Branch with non-Bool condition" ~: TestCase $
      case typeOfStmt testEnv (branch (num 0) [] []) of
        Error (BadAssertType TNum) -> return ()
        _ -> assertFailure "Expected BadAssertType"

  , "Return expression passes (not yet enforced)" ~:
      typeOfStmt testEnv (ret (str "done")) ~?= Ok testEnv
  ]

-- Entry point
main :: IO ()
main = do
  putStrLn "[TEST] Running statement typecheck tests..."
  _ <- runTestTT tests
  return ()
