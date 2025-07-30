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
 Operator (Binary (These (These x y) (Arithmetic _))) -> match env x y `yok` Try `ha_` expect `hv` by Double
 Operator (Binary (These (These x y) (Comparison _))) -> match env x y `yok` Try `ha_` expect `hv` by Double `ho'yu` by Bool
 Operator (Binary (These (These x y) (Combinated _))) -> match env x y `yok` Try `ha_` expect `hv` by Bool
 Operator (Unary (Not x)) -> expression env x `yok` Try `ha_` expect `hv` by Bool
 Operator (Unary (Neg x)) -> expression env x `yok` Try `ha_` expect `hv` by Double
 x -> Error `ha` Unsupported `hv` show x

match env left right =
 intro @(Error Types) Unit
 `yuk____` Try `hv` expression env left
 `lu'yp'yo'q` Try `hv` expression env right
 `yok____` Try `ha__` Error `ha` Mismatched `la` Ok

expect sample typed =
 sample `lu'q` typed `yi_` Error `ha` Unexpected `la` Ok
