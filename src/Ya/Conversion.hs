{-# LANGUAGE PatternSynonyms #-}
module Ya.Conversion where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Ya (type P, Optional, pattern Ok, pattern Error, to, by, ho, hu, la, lu)

find k m = case Map.lookup k m of
 Maybe.Nothing -> Error k
 Maybe.Just x -> Ok x

save :: forall k v . Ord k => k -> v -> Map.Map k v -> k `P` v `P` Map.Map k v
save k v m = k `lu` v `lu` Map.insert k v m
