{-# LANGUAGE PatternSynonyms #-}
module Ya.Conversion where

import Ya (Optional, pattern Some, pattern None, by)

may :: Maybe e -> Optional e
may = maybe (by None) Some
