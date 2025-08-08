module RVRS.Syntax.Primitive where

import Ya

import Prelude (Bool, Double, String)

type Primitive string double bool
 = string `S` double `S` bool

pattern String x = This (This x) :: Primitive string double bool
pattern Double x = This (That x) :: Primitive string double bool
pattern Bool x = That x :: Primitive string double bool

type Value = Primitive String Double Bool

type Typed = Primitive Unit Unit Unit
