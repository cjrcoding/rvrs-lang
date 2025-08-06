{-# LANGUAGE NoImplicitPrelude #-}

module RVRS.Checker where

import Ya
import Ya.Conversion
import Ya.Instances ()

import Data.Map
import Data.String
import Text.Show

import RVRS.AST
import RVRS.Value

type Context = Map String Typed

type Types = (Typed `P` Typed) `S` (Typed `P` Typed) `S` String `S` String

pattern Mismatched x = This (This (This x)) :: Types
pattern Unexpected x = This (This (That x)) :: Types
pattern Unsupported x = This (That x) :: Types
pattern Unknown x = That x :: Types

type Checker = Stops Types `JNT` State Context

expression = is @(Recursive Expression `AR_` Checker Typed)
 `li__` literal `ha'he` is `la` variable `ha'he` is
  `la_` unary `ha'he` is `la` dyadic `ha'he` is

literal = intro @Checker
 `ha__` be `hv'he` String
   `la` be `hv'he` Double
   `la` be `hv'he` Bool

variable x = intro @Checker `hv` Unit
 `yuk____` Lease `hv__` State `ha` Event `hv` get @Context `yo` find x
 `yok____` Check `ha__` Error `ha` Unknown `la` Ok

unary (These operation (Only x)) = intro @Checker `hv` Unit
 `yuk____` Apply `hv__` expression `hv` is @(Recursive Expression) x
 `yok____` Check `ha__` Error `ha` Unexpected `la` Ok
 `ha_____` is @Unary `hv` unwrap operation
     `yi_` Negation `hu` (`lu'q` by Double)
      `la` Complement `hu` (`lu'q` by Bool)

dyadic (These operation (Both (These x y))) = intro @Checker `hv` Unit
 `yuk____` Apply `ha` expression `hv` x
 `lu'yp'yo'q` Apply `ha` expression `hv` y
 `yok____` Check `ha__` Error `ha` Mismatched `la` Ok
 `yok____` Check `ha__` Error `ha` Unexpected `la` Ok
 `ha_____` is @Dyadic `hv` unwrap operation
     `yi_` Arithmetic `hu` (`lu'q` by Double)
      `la` Comparison `hu` (`lu'q` by Double) `ho'ho'ho` (be `hv'he` Bool)
      `la` Combinated `hu` (`lu'q` by Bool)
