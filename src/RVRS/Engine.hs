{-# LANGUAGE NoImplicitPrelude #-}
module RVRS.Engine where

import Prelude (Bool, Double, (+), (-), (*), (/), (&&), (||), (<), (==), (>))
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

type Flowings = Map String Flow

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
 Literal val -> intro @Engine `hv` val
 Variable var -> intro @Engine `hv` Unit
  `yuk____` Old `hv__` State `ha` Event `hv` get @Bindings `yo` find var
  `yok____` Try `ha__` None `hu_` Error `ha` Runtime `hv` Unbound var `la` Ok
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
 -- Calling name args -> intro @Engine `hv` Unit
  -- `yuk____` Run `hv__` Given `hv` is @Flowings `yo` find name
  -- `yok____` Try `ha__` None `hu_` Error `ha` Runtime `hv` Unknown name `la` Ok
  -- `yok____` Run `ha__` 

    -- fsenv <- ask
    -- case Map.lookup name fsenv of
      -- Nothing -> throwError $ RuntimeError ("Unknown function: " ++ name)
      -- Just (FlowIR _ paramNames body) -> do
        -- argVals <- for (toList args) evalExpr
        -- if length paramNames /= length argVals
          -- then throwError $ RuntimeError ("Arity mismatch calling: " ++ name)
          -- else fromJust `ha` fst <$> do callBody (toList body) `ha` fromList $ zip paramNames argVals

tap :: forall target . Value `M` target `S` target `AR___` Error Reason target
tap = Some `hu_` Error `ha` Runtime `ha` Require `hv` Unit `la` Valid @target

-- evalBody stmts = stmts `yokl` Forth `ha` Run `ha` evaluate

evaluate :: Recursive Statement `AR__` Engine Unit
evaluate x = statement x `yok_` Try `ha__` Continue `la` Interrupt `ha` Returns
