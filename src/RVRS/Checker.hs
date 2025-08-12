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

type Mistyped = (Typed `P` Typed) `S` (Typed `P` Typed) `S` (List Typed `P` List Typed) `S` Name

pattern Mismatched x = This (This (This x)) :: Mistyped
pattern Unexpected x = This (This (That x)) :: Mistyped
pattern Misaligned x = This (That x) :: Mistyped
pattern Unknown x = That x :: Mistyped

type Checker = Stops Mistyped `JNT` State Context

expression = is @(Recursive Expression `AR_` Checker Typed)
 `li__` literal `ha'he` is `la` variable `ha'he` is
  `la_` unary `ha'he` is `la` dyadic `ha'he` is
  `la_` calling `ha'he` is

literal = intro @Checker `ha__` be `hv'he` String `la` be `hv'he` Double `la` be `hv'he` Bool

variable var = intro @Checker `hv` Unit
 `yuk____` Lease `ha__` State `hv___` Event `hv` get `ha__` Scope `hv` at @Terms `ho_` Scope `hv` key var
 `yok____` Check `ha__` Error @Mistyped `ha` Unknown `la` Ok @Typed

calling (These name args) = intro @Checker `hv` Unit
 `yuk______` Lease `ha__` State `hv___` Event `hv` get `ha__` Scope `hv` at @Flows `ho_` Scope `ha` key `hv'he` name
 `yok______` Apply `ha__` Exist `hu` args `ho_'yokl` Forth `ha` Apply `ha` expression
 `lo__'yp'yo'q` Check `ha__` Error `ha` Unknown `la` Ok @(List Typed)
 `yok______` Check `ha__` Error `ha` Misaligned `la` Ok @(List Typed)
 `ho_____'yu` Bool Unit

unary (These operation (Only x)) = intro @Checker `hv` Unit
 `yuk____` Apply `hv__` expression `hv` x
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
