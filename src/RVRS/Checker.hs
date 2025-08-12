{-# LANGUAGE NoImplicitPrelude #-}

module RVRS.Checker where

import Ya
import Ya.Conversion
import Ya.Instances ()

import Data.Map
import Data.String
import Text.Show

import RVRS.Syntax
import RVRS.Value

type Terms = Map Name `T` Typed

type Flows = Map Name `T` List Typed

type Context = Terms `P` Flows

type Types = (Typed `P` Typed) `S` (Typed `P` Typed) `S` String `S` Name

pattern Mismatched x = This (This (This x)) :: Types
pattern Unexpected x = This (This (That x)) :: Types
pattern Unsupported x = This (That x) :: Types
pattern Unknown x = That x :: Types

type Checker = Stops Types `JNT` State Context

expression = is @(Recursive Expression `AR_` Checker Typed)
 `li__` literal `ha'he` is `la` variable `ha'he` is
  `la_` unary `ha'he` is `la` dyadic `ha'he` is
  `la_` calling `ha'he` is

literal = intro @Checker `ha__` be `hv'he` String `la` be `hv'he` Double `la` be `hv'he` Bool

variable x = intro @Checker `hv` Unit
 `yuk____` Lease `ha__` State `hv___` Event `hv` get `ha__` Scope `hv` at @Terms `ho_` Scope `hv` key x
 `yok____` Check `ha__` Break @Types `ha` Unknown `la` Ok @Typed

calling :: Instead Name (Recursive Expression) `P` List (Recursive Expression) `AR__` Checker Typed
calling (These name args) = intro @Checker `hv` Unit
 `yuk______` Lease `ha__` State `hv___` Event `hv` get `ha__` Scope `hv` at @Flows `ho_` Scope `ha` key `hv'he` name
 `yok______` Apply `ha__` (\_ -> args `yokl` Forth `ha` Apply `ha` expression)
 `lo__'yp'yo'q` Check `ha__` Break @Types `ha` Unknown `la` Ok @(List Typed)
 -- `yok______` Check `ha__` Error `ha` Mismatched `la` Ok
 -- TODO: Stops Name (List Typed) -> Checker `L` Checker `T` Void `T` (List Typed `P` List Typed `S` List Typed))
 `ho___'yu` Bool Unit

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

-- TODO: implement typechecking of this type of expressions
calling (These name args) = intro @Checker @(AR) `ha` Bool `hv` Unit
 -- `yuk____` Lease `hv__` State `ha` Event `hv` get @Context `yo` find name
