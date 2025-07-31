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

type Checker = State Context `JNT` Stops Types

expression :: Recursive Expression `AR___` Checker Typed
expression x = case unwrap x of
 Literal x -> intro @Checker
  `hv_____` be Unit `ho` String `la` be Unit `ho` Double `la` be Unit `ho` Bool `li` x
 Variable x -> intro @Checker `hv` Unit
  `yuk____` Lease `hv__` State `ha` Event `hv` get @Context `yo` find x
  `yok____` Check `ha__` Error `ha` Unknown `la` Ok
 Operator (Dyadic (These (These x y) operation)) -> intro @Checker `hv` Unit
  `yuk____` Apply `hv__` expression x `lu'yp'yo'q` Apply `hv` expression y
  `yok____` Check `ha__` Error `ha` Mismatched `la` Ok
  `yok____` Check `ha__` Error `ha` Unexpected `la` Ok
  `ha_____` Arithmetic `hu` (`lu'q` Double Unit)
       `la` Comparison `hu` (`lu'q` Double Unit) `ho'ho'ho` be (Bool Unit)
       `la` Combinated `hu` (`lu'q` Bool Unit)
       `li` operation
 Operator (Unary (These x operation)) -> intro @Checker `hv` Unit
  `yuk____` Apply `hv__` expression x
  `yok____` Check `ha__` Error `ha` Unexpected `la` Ok
  `ha_____` Negation `hu` (`lu'q` Double Unit)
       `la` Complement `hu` (`lu'q` Bool Unit)
       `li` operation
