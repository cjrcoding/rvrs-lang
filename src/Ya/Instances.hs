{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ya.Instances where

import Ya (S, P, Object (..), Recursive (..))

deriving instance (Eq (f (Recursive f))) => Eq (Recursive f)
deriving instance (Show (f (Recursive f))) => Show (Recursive f)

deriving instance (Eq l, Eq r) => Eq (l `P` r)
deriving instance (Eq l, Eq r) => Eq (l `S` r)

deriving instance (Show l, Show r) => Show (l `P` r)
deriving instance (Show l, Show r) => Show (l `S` r)
