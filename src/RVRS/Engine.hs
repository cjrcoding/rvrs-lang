{-# LANGUAGE NoImplicitPrelude #-}
module RVRS.Engine where

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

type Runtime = Typed `S` String `S` String

pattern Require e = This (This e)
pattern Unknown e = This (That e)
pattern Unbound e = That e

type Engine = Given Flowings `JNT` State Bindings `JNT` Stops Reason `JNT` World

statement :: Recursive Statement `AR__` Engine `T'I` Optional Value
statement x = case unwrap x of
 Return e -> expression e `yo` Some

expression :: Recursive Expression `AR__` Engine `T'I` Value
expression x = case unwrap x of
 Literal x -> intro @Engine `hv` x
 Variable x -> intro @Engine `hv` Unit
  `yuk____` Old `hv__` State `ha` Event `hv` get @Bindings `yo` find x
  `yok____` Try `ha__` None `hu_` Error `ha` Runtime `hv` Unbound x `la` Ok
 Operator (Binary (Equals x y)) -> intro @Engine `hv` Unit
  `yuk____` Run `hv` expression x `lu'yp'yo'q` Run `hv` expression y
      `yo_` No `hu` False `la` Yes `hu` True `ho_'he` Bool
 -- Operator (Binary (Add x y)) -> intro @Engine `hv` Unit
  -- `yuk____` Run `hv` expression x `lu` Run `hv` expression y
  -- `yp'yo` is @(

-- cast :: (Unit `AR` Typed) `AR_` Value `AR` Stops
-- cast matcher value = 

-- evalBody stmts = stmts `yokl` Forth `ha` Run `ha` evaluate

-- evaluate x = statement x `yok_` Try `ha__` Continue `la` Interrupt `ha` ReturnValue
