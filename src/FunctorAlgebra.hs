module FunctorAlgebra where

data Option a
  = Some a
  | None
  deriving (Show, Eq)

instance Functor Option where
  fmap f (Some a) = Some $ f a
  fmap _ None = None

data Two a b = Two a b deriving Show

-- The structure here is Two a _. fmap cannot touch
-- the a, the first type parameter, since it could
-- alter the structure, and the idea of functor is
-- to leave the structure alone
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

data MyList a
  = Empty
  | Cons a (MyList a)
  deriving Show

instance Functor MyList where
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
