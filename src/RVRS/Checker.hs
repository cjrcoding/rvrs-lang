{-# LANGUAGE NoImplicitPrelude #-}

module RVRS.Checker where

import Ya hiding (Binary)
import Ya.Instances ()

import Data.Map
import Data.String
import Text.Show

import RVRS.AST
import RVRS.Value

type Types = (Typed `P` Typed) `S` (Typed `P` Typed) `S` String `S` String

pattern Mismatched x = This (This (This x)) :: Types
pattern Unexpected x = This (This (That x)) :: Types
pattern Unsupported x = This (That x) :: Types
pattern Unknown x = That x :: Types

expression :: Map String Typed -> Recursive Expression -> Error Types Typed
expression env expr = case unwrap expr of
  Literal x -> Ok `hv` valueToType x
  Variable x -> lookup x env `yi_` None `hu_` Error `hv` Unknown x `la` Ok `ha_` to @Optional
  Operator (Binary (Add x y)) -> binary env x y `yok` Try `ha_` expect `hv` by Double
  Operator (Binary (Sub x y)) -> binary env x y `yok` Try `ha_` expect `hv` by Double
  Operator (Binary (Mul x y)) -> binary env x y `yok` Try `ha_` expect `hv` Double Unit
  Operator (Binary (Div x y)) -> binary env x y `yok` Try `ha_` expect `hv` Double Unit
  Operator (Binary (And x y)) -> binary env x y `yok` Try `ha_` expect `hv` by Bool
  Operator (Binary (Or x y)) -> binary env x y `yok` Try `ha_` expect `hv` by Bool
  Operator (Unary (Not x)) -> expression env x `yok` Try `ha_` expect `hv` by Bool
  Operator (Unary (Neg x)) -> expression env x `yok` Try `ha_` expect `hv` by Double
  Operator (Binary (Equals x y)) -> binary env x y `yu` Bool Unit
  Operator (Binary (Greater x y)) -> binary env x y `yok` Try `ha_` expect `hv` by Double `ho'yu` by Bool
  Operator (Binary (Less x y)) -> binary env x y `yok` Try `ha_` expect `hv` by Double `ho'yu` by Bool
  x -> Error `ha` Unsupported `hv` show x

binary env left right =
 intro @(Error Types) Unit
 `yuk____` Try `hv` expression env left
 `lu'yp'yo'q` Try `hv` expression env right
 `yok____` Try `ha__` Error `ha` Mismatched `la` Ok

expect sample typed =
 Error `ha` Unexpected `la` Ok `li` sample `hd'q` typed
