{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module RVRS.Typecheck.Stmt where

-- ✅ Statement Type-checking Coverage (as of July 23, 2025)
--
-- This checklist tracks progress in `typeOfStmt` and outlines what's implemented,
-- what remains, and what each statement type must enforce.

-- ✅ Delta
--   - RHS expression must type-check successfully
--   - If annotated, ensure annotation matches inferred type

-- ✅ Source
--   - Lookup variable: must already exist in environment
--   - Ensure RHS type matches declared variable type
--   - Enforce immutability (no reassignment to Source-bound vars)
--   - Return updated environment or RedefinedVar error

-- 🟢 Echo
--   - Type-check expression ✔
--   - Returns Unit or passthrough ⬜ (Unit not yet enforced strictly)
--   - Ensure expression is well-typed ✔

-- 🟢 Mouth
--   - Type-check argument expression ✔
--   - Returns Unit or passthrough ⬜
--   - Ensure no side-effect errors ⬜

-- ✅ Assert
--   - Expression must type-check to Bool ✔
--   - Else return BadAssertType error ✔
--   - Assert itself returns Unit (implicit) ✔

-- 🔴 Branch
--   - Condition expression must type-check to Bool
--   - Both blocks type-checked in isolation
--   - Optionally enforce both branches unify in return type
--   - Merge environments correctly

-- 🔴 Return
--   - Type-check expression to match flow’s return type
--   - May require storing expected return type in context
--   - Return type info to caller

-- 🔴 Call
--   - Lookup flow definition by name
--   - Type-check arguments against flow’s parameters
--   - Ensure return type aligns with expected context
--   - Insert any scoped bindings as needed

-- ✅ Fallback
--   - Catch-all `UnsupportedStmt` ensures non-exhaustive cases don’t crash

-- 🧪 Final Tasks
--   • Eliminate “Unhandled statement” test failures (✅ partially done)
--   • Add negative tests for invalid usage:
--     - ✅ Assert on non-Bool
--     - ✅ Branch condition not Bool
--     - ✅ Source reassignment
--     - ⬜ Return with wrong type
--     - ⬜ Calling flow with wrong arity or types
--   • Once all branches covered, remove fallback or move it to end as a guard

-- 🔧 Tip:
-- Use descriptive error constructors for remaining types:
--   e.g., BadBranchCond, BadReturnType, WrongArgCount, FlowNotFound





import Prelude (Eq(..), Show(..), String, Maybe(..), Bool(..), (==), ($), (.), foldl)
import RVRS.AST
import RVRS.Value
import RVRS.Checker

import Ya hiding (Binary)
import Data.Map (Map)
import qualified Data.Map as Map

liftError :: (e1 -> e2) -> Error e1 a -> Error e2 a
liftError f (Ok x)    = Ok x
liftError f (Error e) = Error (f e)

-- Statement-level errors
type StmtTypes = Types `S` (String, Typed, Typed) `S` String `S` Typed

pattern ExprTypeError x     = This (This (This x)) :: StmtTypes
pattern TypeMismatchStmt x  = This (This (That x)) :: StmtTypes
pattern RedefinedVar x      = This (That x)        :: StmtTypes
pattern BadAssertType x     = That x               :: StmtTypes

checkBlock :: Error StmtTypes (Map String Typed) -> Recursive Statement -> Error StmtTypes (Map String Typed)
checkBlock acc stmt = acc `yok` Try `ha` \env' -> typeOfStmt env' stmt

typeOfStmt :: Map String Typed -> Recursive Statement -> Error StmtTypes (Map String Typed)
typeOfStmt env stmt = case unwrap stmt of

  -- Delta with literal and annotation
  Delta name (Just anno) (Recursive (Literal found)) ->
    let foundType = valueToType found in
    if foundType == anno
      then Ok `hv` Map.insert name anno env
      else Error `ha` TypeMismatchStmt `hv` (name, anno, foundType)

  -- Delta with annotation
  Delta name (Just anno) expr ->
    (Error `ha` ExprTypeError `la` Ok `li` expression env expr)
      `yok` Try `ha` \found ->
        if found == anno
          then (Ok `hv` Map.insert name anno env :: Error StmtTypes (Map String Typed))
          else Error `ha` ExprTypeError `ha` Unexpected `hv` (anno `lu` found)

  -- Delta with no annotation
  Delta name Nothing expr ->
    let typedExpr = liftError ExprTypeError (expression env expr)
    in typedExpr `yok` Try `ha` \found ->
         (Ok `hv` Map.insert name found env :: Error StmtTypes (Map String Typed))

  -- Source redefinition check
  Source name _ _ | Map.member name env ->
    Error `ha` RedefinedVar `hv` name

  -- Source with no annotation
  Source name Nothing expr ->
    liftError ExprTypeError (expression env expr)
      `yok` Try `ha` \found ->
        (Ok `hv` Map.insert name found env :: Error StmtTypes (Map String Typed))

  -- Source with annotation
  Source name (Just anno) expr ->
    liftError ExprTypeError (expression env expr)
      `yok` Try `ha` \found ->
        if found == anno
          then (Ok `hv` Map.insert name anno env :: Error StmtTypes (Map String Typed))
          else Error `ha` TypeMismatchStmt `hv` (name, anno, found)

  -- Assert
  Assert expr ->
    let typedExpr = liftError ExprTypeError (expression env expr)
    in typedExpr `yok` Try `ha` \found ->
         if found == Bool Unit
           then Ok `hv` env
           else Error `ha` BadAssertType `hv` found

  -- Branch
  Branch cond thenBlock elseBlock ->
    case liftError ExprTypeError (expression env cond) of
      Error e -> Error e
      Ok condType ->
        case condType of
          Bool Unit ->
            case foldl checkBlock (Ok env) thenBlock of
              Error e1 -> Error e1
              Ok _ ->
                case foldl checkBlock (Ok env) elseBlock of
                  Error e2 -> Error e2
                  Ok _     -> Ok `hv` env
          otherType ->
            Error `ha` TypeMismatchStmt `hv` ("<condition>", Bool Unit, otherType)


  -- Fallback
  _ -> Error `ha` ExprTypeError `ha` Unsupported `hv` "Unhandled statement"