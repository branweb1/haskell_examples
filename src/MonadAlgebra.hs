module MonadAlgebra where

data Option a
  = Some a
  | None
  deriving Show

instance Functor Option where
  fmap f (Some x) = Some $ f x
  fmap _ None = None

instance Applicative Option where
  pure = Some
  (<*>) (Some f) (Some x) = Some $ f x
  (<*>) _ None = None
  (<*>) None _ = None

instance Monad Option where
  return = pure
  (>>=) (Some x) f = f x
  (>>=) None _ = None

data Two a b = Two a b deriving Show

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

-- Here we have a function in some structure, but the
-- structure itself includes some data, namely a, that
-- we must handle in some way when we call ap. To do
-- that generically, we require a to be a monoid. The
-- monoid requirement is also needed by pure, which
-- requries mempty
instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two x f) (Two p q) = Two (mappend x p) (f q)

instance Monoid a => Monad (Two a) where
  return = pure
  (>>=) (Two x y) f =
    let
      (Two p q) = f y
    in
      Two (mappend x p) q 

data MyList a
  = Empty
  | Cons a (MyList a)
  deriving Show

instance Functor MyList where
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (MyList a) where
  (<>) Empty xs = xs
  (<>) xs Empty = xs
  (<>) (Cons x xs) (Cons y ys) = (Cons x (xs <> (Cons y Empty))) <> ys

instance Monoid (MyList a) where
  mappend = (<>)
  mempty = Empty

-- an alternate "zip-ish" implimentation, in which each
-- function is applied to only one element, is also possible
instance Applicative MyList where
  pure a = Cons a Empty
  (<*>) Empty _ = Empty
  (<*>) _ Empty = Empty
  (<*>) (Cons f fs) xs = (fmap f xs) `mappend` (fs <*> xs)

instance Monad MyList where
  return = pure
  (>>=) (Cons x xs) f =
    let
      y = f x
    in
      mappend y (xs >>= f)
  (>>=) Empty _ = Empty

monadDemo :: MyList Int
monadDemo =
  let
    xs = Cons 1 (Cons 2 (Cons 3 Empty))
    f = \n -> Cons (n*2) (Cons (n+2) Empty)
  in
    xs >>= f
