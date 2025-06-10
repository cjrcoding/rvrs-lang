{-# LANGUAGE UndecidableInstances #-}
module Ya.Instances where

import Ya (Recursive (..))

deriving instance (Eq (f (Recursive f))) => Eq (Recursive f)
deriving instance (Show (f (Recursive f))) => Show (Recursive f)
