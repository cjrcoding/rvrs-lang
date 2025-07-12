{-# LANGUAGE PatternSynonyms #-}
module Ya.Conversion where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Ya (Optional, pattern Ok, pattern Error, to, by, ho, hu, la)
import Ya.Instances

find k m = case Map.lookup k m of
 Maybe.Just x -> Ok x
 Maybe.Nothing -> Error k
