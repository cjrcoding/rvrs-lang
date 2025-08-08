{-# LANGUAGE NoImplicitPrelude #-}
module RVRS.Syntax (module Exports, type Flow) where

import RVRS.Syntax.Primitive as Exports
import RVRS.Syntax.Identifier as Exports
import RVRS.Syntax.Operation as Exports
import RVRS.Syntax.Expression as Exports
import RVRS.Syntax.Statement as Exports

import Ya

-- TODO: add optional type annotation (or not optional?)
type Flow = List Name `P` Nonempty List (Recursive Statement)
