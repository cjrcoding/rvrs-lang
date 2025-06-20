{-# LANGUAGE PatternSynonyms #-}
module RVRS.Typecheck.Types where

import RVRS.AST
import Ya

-- Statement-level errors
type StmtTypes = (String, Typed, Typed) `S` String `S` Typed

pattern TypeMismatchStmt x = This (This x) :: StmtTypes
pattern RedefinedVar x = This (That x) :: StmtTypes
pattern BadAssertType x = That x :: StmtTypes
