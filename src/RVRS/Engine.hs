{-# LANGUAGE NoImplicitPrelude #-}
module RVRS.Engine where

import GHC.IsList (fromList, toList)
import Prelude (Double, (+), (-), (*), (/), (<), (==), (>), readFile, putStrLn, error)
import Data.List ((++), zip)
import Data.Bool (Bool (..), bool, (&&), (||))
import Data.String (String)
import Data.Either (Either (..))
import Data.Functor ((<$>))
import Data.Map (Map, insert, union)
import Text.Show (Show (..))
import qualified Data.Map as Map (lookup)

import Ya hiding (Binary, Not, True, False, Left, Right)
import Ya.Conversion
import Ya.World

import RVRS.AST
import RVRS.Parser
import RVRS.Value

type Bindings = Map String Value

type Flowings = Map String Flow

type Reason = Runtime `S` Value

pattern Runtime e = This e :: Reason
pattern Returns e = That e :: Reason

type Runtime = Unit `S` String `S` String `S` (Value `S` String) `S` Recursive Expression `S` String

pattern Require e = This (This (This (This (This e))))
pattern Unknown e = This (This (This (This (That e))))
pattern Unbound e = This (This (This (That e)))
pattern Valency e = This (This (That e))
pattern Neglect e = This (That e)
pattern Defined e = That e

type Engine = Given Flowings `JNT` State Bindings `JNT` Stops Reason `JNT` World

statement :: Recursive Statement `AR__` Engine Value
statement x = case unwrap x of
 Return e -> expression e
 Echo expr -> intro @Engine `hv` Unit
  `yuk____` Apply `hv` expression expr
  `yok____` World `ha` display "echo: "
 Mouth expr -> intro @Engine `hv` Unit
  `yuk____` Apply `hv` expression expr
  `yok____` World `ha` display "mouth: "
 Whisper expr -> intro @Engine `hv` Unit
  `yuk____` Apply `hv` expression expr
  `yok____` World `ha` display "whisper: "
 Call name args -> intro @Engine `hv` Unit
  `yuk____` Apply `hv` params args `lu'yp` Apply `hv` setup name
  `yok____` Check `ha__` unwrap @(AR) `ho_'yoikl` Apply `ha` Check `ha` match
  `yok____` Apply `ha__` calls
 Assert expr -> intro @Engine `hv` Unit
  `yuk____` Apply `hv` expression expr
  `yok____` Check `ha` tap `ha` on @Bool
  `yok____` Check `ha` bool (Error `ha` Runtime `ha` Neglect `hv` expr) (Ok `ha` Bool `hv` True)
 Branch expr if_block else_block -> intro @Engine `hv` Unit
  `yuk____` Apply `hv` expression expr
  `yok____` Check `ha` tap `ha` on @Bool
  `yok____` Ok `hu_` Lease `ha` State `ha` Event `hv` get @Bindings
     `lo'yp` Lease `ha` intro @(State Bindings) @(AR)
  `yok____` Apply `ha` calls `ha'ho` bool if_block else_block
 Delta name _ expr -> intro @Engine `hv` Unit
  `yuk____` Apply `hv` expression expr
  `yok____` Apply `ha` State `ha` Event `ha` save @String @Value name
  `yok____` Check `ha` Break `ha` Returns `ha` that @Value
 Source name _ expr -> intro @Engine `hv` Unit
  `yuk____` Lease `hv___` State `ha` Event `hv` get @Bindings `yo` find name
  `yok____` Check `ha___` Error `hu_` Ok `hv` Unit `la_` Some `hu_` Error `ha` Runtime `ha` Defined `hv` name
  `yuk____` Apply `hv` expression expr
  `yok____` Apply `ha` State `ha` Event `ha` save @String @Value name
  `yok____` Check `ha` Break `ha` Returns `ha` that @Value

expression :: Recursive Expression `AR__` Engine Value
expression x = case unwrap x of
 Literal val -> intro @Engine `hv` val
 Variable var -> intro @Engine `hv` Unit
  `yuk____` Lease `hv__` State `ha` Event `hv` get @Bindings `yo` find var
  `yok____` Check `ha__` Error `ha` Runtime `ha` Unbound `la` Ok
 Operator (Binary (These (These x y) (Comparison operation))) -> intro @Engine `hv` Unit
  `yuk____` Apply `hv` expression x
     `lu'yp` Apply `hv` expression y
  `yok____` Check `ha` tap `ha` on @Double `ha` this
     `lo'yp` Check `ha` tap `ha` on @Double `ha` that
  `ho___'yo` Greater `hu` (is `ho'hd` (>) `ho` Bool)
       `la` Equals `hu` (is `ho'hd` (==) `ho` Bool)
       `la` Less `hu` (is `ho'hd` (<) `ho` Bool)
       `li` is @Comparison operation
 Operator (Binary (These (These x y) (Arithmetic operation))) -> intro @Engine `hv` Unit
  `yuk____` Apply `hv` expression x
     `lu'yp` Apply `hv` expression y
  `yok____` Check `ha` tap `ha` on @Double `ha` this
     `lo'yp` Check `ha` tap `ha` on @Double `ha` that
  `ho___'yo` Add `hu` (is `ho'hd` (+) `ho` Double)
       `la` Sub `hu` (is `ho'hd` (-) `ho` Double)
       `la` Mul `hu` (is `ho'hd` (*) `ho` Double)
       `la` Div `hu` (is `ho'hd` (/) `ho` Double)
       `li` is @Arithmetic operation
 Operator (Binary (These (These x y) (Combinated operation))) -> intro @Engine `hv` Unit
  `yuk____` Apply `hv` expression x
     `lu'yp` Apply `hv` expression y
  `yok____` Check `ha` tap `ha` on @Bool `ha` this
     `lo'yp` Check `ha` tap `ha` on @Bool `ha` that
  `ho___'yo` And `hu` (is `ho'hd` (&&) `ho` Bool)
       `la` Or `hu` (is `ho'hd` (||) `ho` Bool)
       `li` is @Combinated operation
 Calling name args -> intro @Engine `hv` Unit
  `yuk____` Apply `hv` params args `lu'yp` Apply `hv` setup name
  `yok____` Check `ha__` unwrap @AR `ho_'yoikl` Apply `ha` Check `ha` match
  `yok____` Apply `ha__` calls
  `yuk____` Apply `hv__` intro @Engine `hv` Bool True

setup name = intro @Engine @(AR) `hv` Unit
 `yuk____` Apply `hv__` Given `hv` is @Flowings `yo` find name
 `yok____` Check `ha__` Error `ha` Runtime `ha` Unknown `la` Ok @Flow

params args = is @(Nonempty List `T` Recursive Expression)
 args `yokl` Forth `ha` Apply `ha` expression

match :: Nonempty List Value `P` Nonempty List Argument `AR` Stops Reason Bindings
match (These values names) = values `lu'yr` Align `hv` (names `yo` argName)
 `yokl` Apply `ho` Forth `ha__` Error `ha` Runtime `ha` Valency `la` Ok `ha` Equip
 `yo__` to `ha` wrap @(AR) @(Nonempty List `T'TT'I` Equipped String `T'I_` Value)

tap :: forall target . Value `M` target `S` target `AR___` Error Reason target
tap = Some `hu_` Error `ha` Runtime `ha` Require `hv` Unit `la` Valid @target

calls ctxbody = intro @Engine `hv` Unit
 `yuk___` Apply `hv__` Given `hv` is @Flowings
 `yok___` Apply `ha__` block ctxbody
 `yok___` Check `ha__` Error `ha` Runtime `la` Ok

block :: Bindings `P` Nonempty List (Recursive Statement)
 `AR_` Flowings `AR` World `T` Stops Runtime Value
block (These ctx body) flows = body
 `yokl` Forth `ha__` Apply `ha` statement
 `yi__` execute flows ctx
 `yo__` Error `la` Ok `la` Ok `hu` (Ok `hv` Bool True)

execute :: Map String Flow -> Map String Value -> Engine e
 -> World (Stops Reason `T'I` Equipped `T` Map String Value `T` e)
execute flows bindings action = is `hv_'he` action `he'he'hv` flows `he'he'hv` bindings

display label x = putStrLn (label ++ ": " ++ show x) `yu` x

loadAndMergeStdlib :: World `T` Map String Flow
loadAndMergeStdlib = readFile "stdlib/stdlib.rvrs" `yo` parseRVRS `yok` \case
 Left err -> World `ha` error `hv` ("Stdlib parse error:\n" ++ show err)
 Right flows -> World `ha` intro @_ @(AR) `ha` fromList
  `hv` ((\(These flow name) -> (name, flow)) <$> flows)