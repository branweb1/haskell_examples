module SemigroupAlgebra where

import Data.Monoid (Sum, Product)

data Option a
  = Some a
  | None
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Option a) where
  (<>) (Some x) (Some y) = Some $ x <> y
  (<>) None _ = None
  (<>) _ None = None

sums :: Option (Sum Int)
sums =
  let
    x = Some 4
    y = Some 5
  in
    x <> y

prods :: Option (Product Int)
prods =
  let
    x = Some 4
    y = Some 5
  in
    x <> y

data Try a b
  = Failure a
  | Success b
  deriving (Show, Eq)

-- Either behaves differently
instance (Semigroup a, Semigroup b) => Semigroup (Try a b) where
  (<>) (Success x) (Success y) = Success $ x <> y
  (<>) (Failure x) (Failure y) = Failure $ x <> y
  (<>) f (Success _) = f
  (<>) (Success _) f = f

tryDemo :: (Try String (Sum Int), Try String (Sum Int))
tryDemo =
  let
    x = Success 3
    y = Success 4
    a = Failure "foo"
    b = Failure "bar"
  in
    (x <> y, a <> b)
