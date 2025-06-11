{-# LANGUAGE PatternSynonyms #-}
module Ya.Conversion where

import Ya (Optional, pattern Some, pattern None, by, hu, la)

may :: Maybe e -> Optional e
may = maybe (by None) Some

toMaybe :: Optional e -> Maybe e
toMaybe = None `hu` Nothing `la` Just
