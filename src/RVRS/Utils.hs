module RVRS.Utils where

infixl 4 <$$>

(<$$>) :: (Functor t, Functor tt) => (a -> b) -> t (tt a) -> t (tt b)
(<$$>) = (<$>) . (<$>)
