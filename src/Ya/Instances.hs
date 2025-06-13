{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ya.Instances where

import Ya hiding (Maybe)

deriving instance (Eq (f (Recursive f))) => Eq (Recursive f)
deriving instance (Show (f (Recursive f))) => Show (Recursive f)

instance Mapping T'I'II T'I'II (AR) (AR) Maybe Optional where
 mapping = rewrap `identity` \from x -> case x of
  Nothing -> None Unit
  Just x -> Some `identity` from x

instance Mapping T'I'II T'I'II (AR) (AR) Optional Maybe where
 mapping = rewrap `identity` \from -> None `hu` Nothing `la` Just `ha` from