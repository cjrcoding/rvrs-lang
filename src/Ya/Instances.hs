{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ya.Instances where

import Ya hiding (Maybe)
import Ya (S, P, Object (This, That, These), Recursive (Recursive), Setoid (equality))
import Ya.Literal ()

import GHC.IsList (fromList, toList)
import Data.Map (Map)

-- Deriving instances for Recursive, P, and S
deriving instance (Eq (f (Recursive f))) => Eq (Recursive f)
deriving instance (Show (f (Recursive f))) => Show (Recursive f)

deriving instance (Eq l, Eq r) => Eq (l `P` r)
deriving instance (Eq l, Eq r) => Eq (l `S` r)

deriving instance (Eq l, Eq r) => Eq (T'I'II P l r)
deriving instance (Eq l, Eq r) => Eq (T'I'II S l r)

deriving instance (Eq l, Eq r) => Eq (T'II'I P r l)
deriving instance (Eq l, Eq r) => Eq (T'II'I S r l)

deriving instance (Eq (tt i (t ii)), Eq i, Eq ii) => Eq (TT'I'T'II t tt i ii)
deriving instance (Eq i) => Eq (Construction Optional i)

deriving instance (Eq i) => Eq (Tagged tag i)

deriving instance (Show l, Show r) => Show (l `P` r)
deriving instance (Show l, Show r) => Show (l `S` r)

deriving instance (Show l, Show r) => Show (T'I'II P l r)
deriving instance (Show l, Show r) => Show (T'I'II S l r)

deriving instance (Show l, Show r) => Show (T'II'I P r l)
deriving instance (Show l, Show r) => Show (T'II'I S r l)

deriving instance (Show (tt i (t ii)), Show i, Show ii) => Show (TT'I'T'II t tt i ii)
deriving instance (Show i) => Show (Construction Optional i)

deriving instance (Show i) => Show (Tagged tag i)

instance Setoid AR String where equality (These x y) = if x == y then That y else This (These x y)
instance Setoid AR Double where equality (These x y) = if x == y then That y else This (These x y)

-- Mapping instances for Optional <-> Maybe
instance Mapping T'I'II T'I'II (AR) (AR) Maybe Optional where
  mapping = rewrap `identity` \from x -> case x of
    Nothing -> None Unit
    Just x  -> Some `identity` from x

instance Mapping T'I'II T'I'II (AR) (AR) Optional Maybe where
  mapping = rewrap `identity` \from -> None `hu` Nothing `la` Just `ha` from

-- Nonempty List `T'TT'I` Equipped key `T'I_` item -----> Map key item
instance Ord key => Mapping T'I'II T'I'II (AR) (AR) (Construction Optional `T'TT'I` Equipped key) (Map key) where
  mapping = rewrap `identity` \from x -> unwrap x `yo` unwrap @(AR) `ho` (\(These v k) -> (k, from v)) `yi` toList `ho` fromList

-- List `T'TT'I` Equipped key `T'I_` item -----> Map key item
instance Ord key => Mapping T'I'II T'I'II (AR) (AR) (List `T'TT'I` Equipped key) (Map key) where
  mapping = rewrap `identity` \from x -> unwrap x `yo` unwrap @(AR) `ho` (\(These v k) -> (k, from v)) `yi` toList `ho` fromList
