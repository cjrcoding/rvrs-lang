{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module RVRS.Typecheck.Stmt where

-- TODO: Statement Type-checking Coverage
--
-- This checklist tracks which statement forms are covered in `typeOfStmt`
-- and what logic still needs to be implemented.
--
-- ✅ Delta
--   - RHS expression type-checks
--   - Annotation match enforced
--
-- 🔜 Source
--   - Ensure var exists & immutability
--
-- 🔜 Echo
-- 🔜 Mouth
--
-- 🔜 Assert
--   - Must evaluate to Bool
--
-- 🔜 Branch
--   - Bool condition, type-check both blocks
--
-- 🔜 Return
--   - Match flow’s expected return type
--
-- 🔜 Call
--   - Arg/return type validation
--
-- ✅ Fallback
--   - Catch-all `Unsupported` to avoid crashes
--
-- Final tasks
--   • Eliminate remaining “Unhandled statement” test failures
--   • Add negative tests (assert non-Bool, branch mismatches, etc.)



import Prelude (Eq(..), Show(..), String, Maybe (..), Bool, (==), ($), (.), foldl)
import RVRS.AST
import RVRS.Value
import RVRS.Checker

import Ya hiding (Binary)
import Data.Map (Map)
import qualified Data.Map as Map

liftError :: (e1 -> e2) -> Error e1 a -> Error e2 a
liftError f (Ok x)   = Ok x
liftError f (Error e) = Error (f e)

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

  Delta name Nothing expr ->
    let typedExpr :: Error StmtTypes Typed
        typedExpr = liftError ExprTypeError (expression env expr)
    in typedExpr
        `yok` Try `ha` \found ->
            (Ok `hv` (Map.insert name found env) :: Error StmtTypes (Map String Typed))


  Assert expr ->
    let typedExpr :: Error StmtTypes Typed
        typedExpr = liftError ExprTypeError (expression env expr)
    in typedExpr `yok` Try `ha` \found ->
         if found == Bool Unit
           then Ok `hv` env
           else Error `ha` BadAssertType `hv` found
  _ -> Error `ha` ExprTypeError `ha` Unsupported `hv` "Unhandled statement"






-- typeOfBlock :: Map String Typed -> [Recursive Statement] -> Error StmtTypes (Map String Typed)
-- typeOfBlock initial = foldl step (Ok initial)
  -- where
    -- step acc stmt = acc `yok` Try `ha__` \env -> typeOfStmt env stmt

