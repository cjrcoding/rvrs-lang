{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude (String, Double, Bool(..), Maybe (..), IO, ($), (>>), putStrLn, return, show, Eq(..), (++))
import Test.HUnit
import qualified Data.Map as Map

import Ya (pattern Ok, pattern Error, pattern Unit, pattern That, pattern This)
import Ya (Recursive(..), ho, ho'ho, hv)
import Ya.Instances ()

import RVRS.AST
import RVRS.Typecheck.Stmt

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

assert_ :: Recursive Expression -> Recursive Statement
assert_ = Assert `ho` Recursive

-- branch :: Recursive Expression -> [Recursive Statement] -> [Recursive Statement] -> Recursive Statement
-- branch cond tb fb = Recursive `hv` Branch cond tb fb

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
        Error (TypeMismatchStmt ("z", TBool, TNum)) -> return ()
        _ -> assertFailure "Expected TypeMismatchStmt"

  , "Source redefinition error" ~: TestCase $
      case typeOfStmt testEnv (source "x" (Just TNum) (num 1)) of
        Error (RedefinedVar "x") -> return ()
        _ -> assertFailure "Expected RedefinedVar"

  , "Valid assert on Bool" ~:
      typeOfStmt testEnv (assert_ (bool True)) ~?= Ok testEnv

  , "Invalid assert on Num" ~: TestCase $
      case typeOfStmt testEnv (assert_ (num 99)) of
        Error (BadAssertType TNum) -> return ()
        _ -> assertFailure "Expected BadAssertType"

  -- , "Branch with Bool condition" ~:
      -- typeOfStmt testEnv (branch (bool True)
        -- [delta "z" (Just TNum) (num 1)]
        -- [delta "z" (Just TNum) (num 2)])
        -- ~?= Ok testEnv

  -- , "Branch with non-Bool condition" ~: TestCase $
      -- case typeOfStmt testEnv (branch (num 0) [] []) of
        -- Error (TypeMismatchStmt ("<condition>", TBool, TNum)) -> return ()
        -- _ -> assertFailure "Expected TypeMismatchStmt for non-Bool branch condition"

  , "Return not yet supported (expect fallback error)" ~: TestCase $
      case typeOfStmt testEnv (ret (str "done")) of
       Error (ExprTypeError (This (That msg))) | msg == "Unhandled statement" -> return ()
       other -> assertFailure $ "Unexpected error: " ++ show other

  -- , "Branch with valid Bool condition (explicit test)" ~:
      -- typeOfStmt testEnv (branch (bool True)
        -- [delta "z" (Just TNum) (num 1)]
        -- [delta "z" (Just TNum) (num 2)])
        -- ~?= Ok testEnv

  -- , "Branch with non-Bool condition (should fail)" ~: TestCase $
      -- case typeOfStmt testEnv (branch (num 0)
        -- [delta "a" (Just TNum) (num 1)]
        -- [delta "a" (Just TNum) (num 2)]) of
        -- Error (TypeMismatchStmt ("<condition>", TBool, TNum)) -> return ()
        -- _ -> assertFailure "Expected TypeMismatchStmt for non-Bool branch condition"
  ]

-- Entry point
main :: IO ()
main = do
  putStrLn "[TEST] Running statement typecheck tests..."
  _ <- runTestTT tests
  return ()
