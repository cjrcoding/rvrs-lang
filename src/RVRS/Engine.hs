{-# LANGUAGE NoImplicitPrelude #-}
module RVRS.Engine where

import Prelude (Double, (+), (-), (*), (/))
import Data.List ((++))
import Data.String (String)
import Data.Map (Map, insert, union)
import qualified Data.Map as Map (lookup)

import Ya hiding (Binary, Not)
import Ya.Conversion
import Ya.World

import RVRS.AST
import RVRS.Parser
import RVRS.Value
import RVRS.Lower

type Bindings = Map String Value

type Flowings = Map String FlowIR

type Stops = Error

type Reason = Runtime `S` Value

pattern Runtime e = This e :: Reason
pattern Returns e = That e :: Reason

-- type Runtime = Value `P` Typed `S` String `S` String
type Runtime = Unit `S` String `S` String

pattern Require e = This (This e)
pattern Unknown e = This (That e)
pattern Unbound e = That e

type Engine = Given Flowings `JNT` State Bindings `JNT` Stops Reason `JNT` World

statement :: Recursive Statement `AR__` Engine `T'I` Optional Value
statement x = case unwrap x of
 Return e -> expression e `yo` Some

expression :: Recursive Expression `AR__` Engine `T'I` Value
expression x = case unwrap x of
 Literal val
  -> intro @Engine `hv` val
 Variable var
  -> intro @Engine `hv` Unit
   `yuk____` Old `hv__` State `ha` Event `hv` get @Bindings `yo` find var
   `yok____` Try `ha__` None `hu_` Error `ha` Runtime `hv` Unbound var `la` Ok
 Operator (Binary (These (These x y) (Comparison (Equals _))))
  -> intro @Engine `hv` Unit
   `yuk____` Run `hv` expression x
   `lu'yp'yo'q` Run `hv` expression y
     `yo'yuu` Unit `yo` Boolean `ho` Bool
 Operator (Binary (These (These x y) (Arithmetic operation)))
  -> intro @Engine `hv` Unit
   `yuk____` Run `hv` expression x
      `lu'yp` Run `hv` expression y
   `yok____` Try `ha` tap `ha` on @Double `ha` this
      `lo'yp` Try `ha` tap `ha` on @Double `ha` that
   `ho___'yo` Add `hu` (is `ho'hd` (+) `ho` Double)
        `la` Sub `hu` (is `ho'hd` (-) `ho` Double)
        `la` Mul `hu` (is `ho'hd` (*) `ho` Double)
        `la` Div `hu` (is `ho'hd` (/) `ho` Double)
        `li` is @Arithmetic operation

tap = Some `hu_` Error `ha` Runtime `ha` Require `hv` Unit `la` Valid

-- evalBody stmts = stmts `yokl` Forth `ha` Run `ha` evaluate

-- evaluate x = statement x `yok_` Try `ha__` Continue `la` Interrupt `ha` ReturnValue
