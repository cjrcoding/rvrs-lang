{-# LANGUAGE PatternSynonyms #-}
module Ya.Conversion where

import qualified Data.Map as Map

import Ya (Optional, pattern Some, pattern None, to, by, ho, hu, la)
import Ya.Instances

find k = Map.lookup k `ho` to @Optional
