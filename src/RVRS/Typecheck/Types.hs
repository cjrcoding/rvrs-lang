{-# LANGUAGE PatternSynonyms #-}
module RVRS.Typecheck.Types where

import RVRS.AST
import Ya

-- Statement-level errors
type StmtTypes = (String, Typed, Typed) `S` String `S` String

pattern TypeMismatchStmt var expected actual = This (This (var, expected, actual)) :: StmtTypes
pattern RedefinedVar var = This (That var) :: StmtTypes
pattern BadAssertType found = That found :: StmtTypes
