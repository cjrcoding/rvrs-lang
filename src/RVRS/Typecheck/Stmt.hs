{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module RVRS.Typecheck.Stmt where

import Prelude (Eq(..), Show(..), String, Maybe (..), Bool, (==), ($), (.), foldl)
import RVRS.AST
import RVRS.Value
import RVRS.Checker (expression, pattern Unexpected)
import RVRS.Typecheck.Types
  ( StmtTypes(..)
  , pattern TypeMismatchStmt
  , pattern RedefinedVar
  , pattern BadAssertType
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
      else Error `ha` TypeMismatchStmt `hv` (name, anno, foundType)

  -- Delta name (Just anno) expr ->
    -- expression env expr `yok` Try `ha__` \found ->
      -- if found == anno
        -- then Ok `hv` Map.insert name anno env
        -- else Error `ha` Unexpected `hv` show (anno, found)

  -- Delta name Nothing expr ->
    -- expression env expr `yok` Try `ha__` \found ->
      -- Ok `hv` Map.insert name found env

  -- _ -> Error (UnsupportedStmt (show stmt))

-- typeOfBlock :: Map String Typed -> [Recursive Statement] -> Error StmtTypes (Map String Typed)
-- typeOfBlock initial = foldl step (Ok initial)
  -- where
    -- step acc stmt = acc `yok` Try `ha__` \env -> typeOfStmt env stmt
