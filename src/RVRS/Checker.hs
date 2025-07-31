{-# LANGUAGE NoImplicitPrelude #-}

module RVRS.Checker where

import Ya hiding (Binary)
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
 Operator (Binary (These (These x y) operation)) -> intro @Checker `hv` Unit
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

-- expression :: Map String Typed -> Recursive Expression -> Error Types Typed
-- expression env expr = case unwrap expr of
 -- Literal x -> Ok `hv` valueToType x
 -- Variable x -> lookup x env `yi_` None `hu_` Error `hv` Unknown x `la` Ok `ha_` to @Optional
 -- Operator (Binary (These (These x y) (Arithmetic _))) -> match env x y `yok` Try `ha_` expect `hv` by Double
 -- Operator (Binary (These (These x y) (Comparison _))) -> match env x y `yok` Try `ha_` expect `hv` by Double `ho'yu` by Bool
 -- Operator (Binary (These (These x y) (Combinated _))) -> match env x y `yok` Try `ha_` expect `hv` by Bool
 -- Operator (Unary (These x (Complement _))) -> expression env x `yok` Try `ha_` expect `hv` by Bool
 -- Operator (Unary (These x (Negation _))) -> expression env x `yok` Try `ha_` expect `hv` by Double
 -- x -> Error `ha` Unsupported `hv` show x
-- 
-- match env left right =
 -- intro @(Error Types) Unit
 -- `yuk____` Try `hv` expression env left
 -- `lu'yp'yo'q` Try `hv` expression env right
 -- `yok____` Try `ha__` Error `ha` Mismatched `la` Ok
-- 
-- expect sample typed =
 -- sample `lu'q` typed `yi_` Error `ha` Unexpected `la` Ok
