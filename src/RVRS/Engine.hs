{-# LANGUAGE NoImplicitPrelude #-}
module RVRS.Engine where

import Data.List ((++))
import Data.String (String)
import Data.Map (Map, insert, union)
import qualified Data.Map as Map (lookup)

import Ya
import Ya.World

import RVRS.AST
import RVRS.Utils
import RVRS.Value
import RVRS.Parser
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
  `yuk_` Old `hv__` State `ha` Event `hv` get `yo` Map.lookup x `ho` to @Optional
  `yok_` Try `ha__` None `hu_` Error `ha` Runtime `hv` Unbound x `la` Ok

 -- Equals a b -> expression a `lu'yp` Run `hv` expression b
 --  `yo` (\(These a' b') -> VPrim `ha` Bool `hv` (a' == b'))

-- evalBody stmts = stmts `yokl` Forth `ha` Run `ha` evaluate

-- evaluate x = statement x `yok_` Try `ha__` Continue `la` Interrupt `ha` ReturnValue