{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module RVRS.Typecheck.Stmt where

-- TODO: Statement Typechecking Coverage
--
-- This checklist tracks which statement forms are covered in `typeOfStmt`
-- and what logic still needs to be implemented. Aim is full case coverage
-- and meaningful error reporting for incorrect usage.

-- Delta
--   - Ensure right-hand side (RHS) expression typechecks
--   - If a type annotation exists, ensure it matches the inferred type

-- Source
--   - Ensure variable already exists in scope
--   - Ensure RHS type matches the declared type of the variable
--   - Enforce immutability: cannot reassign a Source-defined variable

-- Echo
--   - Typecheck the expression being echoed
--   - Likely returns Unit or allows passthrough

-- Mouth
--   - Typecheck argument expression
--   - May act as passthrough or produce Unit

-- Assert
--   - Ensure the expression evaluates to a Bool
--   - Otherwise, return a type error indicating misuse

-- Branch
--   - Condition expression must typecheck to Bool
--   - Typecheck both branches independently
--   - Optionally enforce matching return types across branches

-- Return
--   - Ensure the returned expression type aligns with expected flow return type
--   - May require deferring type until flow type is known

-- Call
--   - Lookup flow by name
--   - Ensure number and types of arguments match parameters
--   - Typecheck returned value as expected type of the call

-- Fallback
--   - Add a catch-all case with `UnsupportedStmt` to avoid crashes

-- Final Tasks
--   - Eliminate all "non-exhaustive pattern" runtime errors
--   - Ensure all `testtypecheckstmt` tests either pass or fail with meaningful errors
--   - Add new cases for invalid usage: assert with non-Bool, branch with mismatched returns, etc.


import Prelude (Eq(..), Show(..), String, Maybe (..), Bool, (==), ($), (.), foldl)
import RVRS.AST
import RVRS.Value
import RVRS.Checker

import Ya hiding (Binary)
import Data.Map (Map)
import qualified Data.Map as Map

-- Statement-level errors
type StmtTypes = Types `S` (String, Typed, Typed) `S` String `S` Typed

pattern ExprTypeError x = This (This (This x)) :: StmtTypes
pattern TypeMismatchStmt x = This (This (That x)) :: StmtTypes
pattern RedefinedVar x = This (That x) :: StmtTypes
pattern BadAssertType x = That x :: StmtTypes

typeOfStmt :: Map String Typed -> Recursive Statement -> Error StmtTypes (Map String Typed)
typeOfStmt env stmt = case unwrap stmt of
  Delta name (Just anno) (Recursive (Literal found)) ->
    let foundType = valueToType found in
    if foundType == anno
      then Ok `hv` Map.insert name anno env
      else Error `ha` TypeMismatchStmt `hv` (name, anno, foundType)

  Delta name (Just anno) expr ->
    (Error `ha` ExprTypeError `la` Ok `li` expression env expr) `yok` Try `ha` (\found ->
      if found == anno
        then Ok `hv` Map.insert name anno env
        else Error `ha` ExprTypeError `ha` Unexpected `hv` (anno `lu` found))

  -- Delta name Nothing expr ->
    -- expression env expr `yok` Try `ha__` \found ->
      -- Ok `hv` Map.insert name found env

  -- _ -> Error (UnsupportedStmt (show stmt))

-- typeOfBlock :: Map String Typed -> [Recursive Statement] -> Error StmtTypes (Map String Typed)
-- typeOfBlock initial = foldl step (Ok initial)
  -- where
    -- step acc stmt = acc `yok` Try `ha__` \env -> typeOfStmt env stmt
