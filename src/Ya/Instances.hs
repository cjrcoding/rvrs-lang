{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ya.Instances where

import Ya hiding (Maybe)
import Ya (S, P, Object (..), Recursive (..))

-- Deriving instances for Recursive, P, and S
deriving instance (Eq (f (Recursive f))) => Eq (Recursive f)
deriving instance (Show (f (Recursive f))) => Show (Recursive f)

deriving instance (Eq l, Eq r) => Eq (l `P` r)
deriving instance (Eq l, Eq r) => Eq (l `S` r)

deriving instance (Eq l, Eq r) => Eq (T'I'II P l r)
deriving instance (Eq l, Eq r) => Eq (T'I'II S l r)

deriving instance (Eq l, Eq r) => Eq (T'II'I P r l)
deriving instance (Eq l, Eq r) => Eq (T'II'I S r l)

deriving instance (Show l, Show r) => Show (l `P` r)
deriving instance (Show l, Show r) => Show (l `S` r)

deriving instance (Show l, Show r) => Show (T'I'II P l r)
deriving instance (Show l, Show r) => Show (T'I'II S l r)

deriving instance (Show l, Show r) => Show (T'II'I P r l)
deriving instance (Show l, Show r) => Show (T'II'I S r l)

-- Mapping instances for Optional <-> Maybe
instance Mapping T'I'II T'I'II (AR) (AR) Maybe Optional where
  mapping = rewrap `identity` \from x -> case x of
    Nothing -> None Unit
    Just x  -> Some `identity` from x

instance Mapping T'I'II T'I'II (AR) (AR) Optional Maybe where
  mapping = rewrap `identity` \from -> None `hu` Nothing `la` Just `ha` from
