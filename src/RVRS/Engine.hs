{-# LANGUAGE NoImplicitPrelude #-}
module RVRS.Engine where

import Data.List ((++))
import Data.String (String)
import Data.Map (Map, insert, union)
import qualified Data.Map as Map (lookup)

import Ya
import Ya.World

import RVRS.AST
import RVRS.Env
import RVRS.Utils
import RVRS.Value
import RVRS.Parser
import RVRS.Lower

type Env = Map String Value
type FlowEnv = Map String FlowIR

data EvalError
  = RuntimeError String
  | ReturnValue Value
  -- deriving (Show, Eq)

type Engine = Given FlowEnv `JNT` State ValueEnv `JNT` Error EvalError `JNT` World

statement :: Recursive Statement `AR__` Engine `T'I` Optional Value
statement x = case unwrap x of
 Return e -> expression e `yo` Some

expression :: Recursive Expression `AR__` Engine `T'I` Value
expression x = case unwrap x of
 Lit x -> intro @Engine `ha` VPrim `hv` x
 Var name -> intro @Engine `hv` Unit
  `yuk_` Old `hv__` State `ha` Event `hv` get `yo` Map.lookup name `ho` to @Optional
  `yok_` Try `ha__` None `hu` Error (RuntimeError ("Unbound variable: " ++ name)) `la` Ok
 -- Equals a b -> expression a `lu'yp` Run `hv` expression b
 --  `yo` (\(These a' b') -> VPrim `ha` Bool `hv` (a' == b'))

-- evalBody stmts = stmts `yokl` Forth `ha` Run `ha` evaluate

-- evaluate x = statement x `yok_` Try `ha__` Continue `la` Interrupt `ha` ReturnValue