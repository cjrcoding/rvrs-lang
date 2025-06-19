{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module RVRS.Typecheck.Stmt where

import Prelude (Eq(..), Show(..), String, Bool, (==), ($), (.), foldl)
import RVRS.AST
import RVRS.Value
import RVRS.Checker (expression)
import RVRS.Typecheck.Types
  ( Typed, StmtTypes(..)
  , pattern TypeMismatchStmt
  , pattern RedefinedVar
  , pattern BadAssertType
  , pattern Unexpected      
  , pattern UnsupportedStmt
  )
import Ya hiding (Binary)
import Data.Map (Map)
import qualified Data.Map as Map

typeOfStmt :: Map String Typed -> Recursive Statement -> Error StmtTypes (Map String Typed)
typeOfStmt env stmt = case unwrap stmt of
  Delta name (Just anno) (Recursive (Literal found)) ->
    let foundType = valueToType found in
    if foundType == anno
      then Ok `hv` Map.insert name anno env
      else Error `ha` Unexpected `hv` show (anno, foundType)

  Delta name (Just anno) expr ->
    expression env expr `yok` Try `ha__` \found ->
      if found == anno
        then Ok `hv` Map.insert name anno env
        else Error `ha` Unexpected `hv` show (anno, found)

  Delta name Nothing expr ->
    expression env expr `yok` Try `ha__` \found ->
      Ok `hv` Map.insert name found env

  _ -> Error (UnsupportedStmt (show stmt))

typeOfBlock :: Map String Typed -> [Recursive Statement] -> Error StmtTypes (Map String Typed)
typeOfBlock initial = foldl step (Ok initial)
  where
    step acc stmt = acc `yok` Try `ha__` \env -> typeOfStmt env stmt
