module RVRS.Utils where

infixl 4 <$$>, <$$$>

(<$$>) :: (Functor t, Functor tt) => (a -> b) -> t (tt a) -> t (tt b)
(<$$>) = (<$>) . (<$>)


(<$$$>) :: (Functor t, Functor tt, Functor ttt) => (a -> b) -> t (tt (ttt a)) -> t (tt (ttt b))
(<$$$>) = (<$>) . (<$>) . (<$>)
