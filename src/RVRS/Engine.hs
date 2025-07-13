{-# LANGUAGE NoImplicitPrelude #-}
module RVRS.Engine where

import Prelude (Double, (+), (-), (*), (/), (<), (==), (>))
import Data.List ((++))
import Data.Bool (Bool (..), bool, (&&), (||))
import Data.String (String)
import Data.Map (Map, insert, union)
import qualified Data.Map as Map (lookup)

import Ya hiding (Binary, Not, True, False)
import Ya.Conversion
import Ya.World

import RVRS.AST
import RVRS.Parser
import RVRS.Value

type Bindings = Map String Value

type Flowings = Map String Flow

type Stops = Error

type Reason = Runtime `S` Value

pattern Runtime e = This e :: Reason
pattern Returns e = That e :: Reason

type Runtime = Unit `S` String `S` String `S` (Value `S` String) `S` Recursive Expression

pattern Require e = This (This (This (This e)))
pattern Unknown e = This (This (This (That e)))
pattern Unbound e = This (This (That e))
pattern Valency e = This (That e)
pattern Neglect e = That e

type Engine = Given Flowings `JNT` State Bindings `JNT` Stops Reason `JNT` World

statement :: Recursive Statement `AR__` Engine `T'I` Optional Value
statement x = case unwrap x of
 Return e -> expression e `yo` Some
 Call name args -> intro @Engine `hv` Unit
  `yuk____` Run `hv` params args `lu'yp` Run `hv` setup name
  `yok____` Try `ha__` unwrap @(AR) `ho_'yoikl` Run `ha` Try `ha` match
  `yok____` Run `ha__` calls
  `yuk____` Run `hv__` intro @Engine `ha` None `hv` Unit
 Assert expr -> intro @Engine `hv` Unit
  `yuk____` Run `hv` expression expr
  `yok____` Try `ha` tap `ha` on @Bool
  `yok____` Try `ha` assert expr

expression :: Recursive Expression `AR__` Engine `T'I` Value
expression x = case unwrap x of
 Literal val -> intro @Engine `hv` val
 Variable var -> intro @Engine `hv` Unit
  `yuk____` Old `hv__` State `ha` Event `hv` get @Bindings `yo` find var
  `yok____` Try `ha__` Error `ha` Runtime `ha` Unbound `la` Ok
 Operator (Binary (These (These x y) (Comparison operation))) -> intro @Engine `hv` Unit
  `yuk____` Run `hv` expression x
     `lu'yp` Run `hv` expression y
  `yok____` Try `ha` tap `ha` on @Double `ha` this
     `lo'yp` Try `ha` tap `ha` on @Double `ha` that
  `ho___'yo` Greater `hu` (is `ho'hd` (>) `ho` Bool)
       `la` Equals `hu` (is `ho'hd` (==) `ho` Bool)
       `la` Less `hu` (is `ho'hd` (<) `ho` Bool)
       `li` is @Comparison operation
 Operator (Binary (These (These x y) (Arithmetic operation))) -> intro @Engine `hv` Unit
  `yuk____` Run `hv` expression x
     `lu'yp` Run `hv` expression y
  `yok____` Try `ha` tap `ha` on @Double `ha` this
     `lo'yp` Try `ha` tap `ha` on @Double `ha` that
  `ho___'yo` Add `hu` (is `ho'hd` (+) `ho` Double)
       `la` Sub `hu` (is `ho'hd` (-) `ho` Double)
       `la` Mul `hu` (is `ho'hd` (*) `ho` Double)
       `la` Div `hu` (is `ho'hd` (/) `ho` Double)
       `li` is @Arithmetic operation
 Operator (Binary (These (These x y) (Combinated operation))) -> intro @Engine `hv` Unit
  `yuk____` Run `hv` expression x
     `lu'yp` Run `hv` expression y
  `yok____` Try `ha` tap `ha` on @Bool `ha` this
     `lo'yp` Try `ha` tap `ha` on @Bool `ha` that
  `ho___'yo` And `hu` (is `ho'hd` (&&) `ho` Bool)
       `la` Or `hu` (is `ho'hd` (||) `ho` Bool)
       `li` is @Combinated operation
 Calling name args -> intro @Engine `hv` Unit
  `yuk____` Run `hv` params args `lu'yp` Run `hv` setup name
  `yok____` Try `ha__` unwrap @AR `ho_'yoikl` Run `ha` Try `ha` match
  `yok____` Run `ha__` calls
  `yuk____` Run `hv__` intro @Engine `hv` Bool True

setup name = intro @Engine @(AR) `hv` Unit
 `yuk____` Run `hv__` Given `hv` is @Flowings `yo` find name
 `yok____` Try `ha__` Error `ha` Runtime `ha` Unknown `la` Ok @Flow

params args = is @(Nonempty List `T` Recursive Expression)
 args `yokl` Forth `ha` Run `ha` expression

match :: Nonempty List Value `P` Nonempty List Argument `AR` Stops Reason Bindings
match (These values names) = values `lu'yr` Align `hv` (names `yo` argName)
 `yokl` Run `ho` Forth `ha__` Error `ha` Runtime `ha` Valency `la` Ok `ha` Equip
 `yo__` to `ha` wrap @(AR) @(Nonempty List `T'TT'I` Equipped String `T'I_` Value)

tap :: forall target . Value `M` target `S` target `AR___` Error Reason target
tap = Some `hu_` Error `ha` Runtime `ha` Require `hv` Unit `la` Valid @target

calls :: Bindings `P` (Nonempty List `T` Recursive Statement) `AR` Engine Bindings
calls (These ctx body) = intro @Engine `hv` Unit
 `yuk___` New `ha` State `ha` Event `ha` put @Bindings `hv` ctx
 `yok___` Ok `hu_` Run `hv` block body `lo'yp` Run `ha` intro @Engine @(AR)
 `yok___` New `ha` State `ha` Event `ha` put @Bindings `ha` that

block :: Nonempty List `T` Recursive Statement `AR___` Engine (Nonempty List Unit)
block stmts = stmts `yokl` Forth `ha` Run `ha` evaluate

string = "TEST"

evaluate :: Recursive Statement `AR__` Engine Unit
evaluate x = statement x `yok_` Try `ha__` Continue `la` Interrupt `ha` Returns

assert expr = bool (Error `ha` Runtime `ha` Neglect `hv` expr) (Ok `ha` None `hv` Unit)