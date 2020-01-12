module MonoidAlgebra where

import Data.Monoid (Sum, Product)

data Option a
  = Some a
  | None
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Option a) where
  (<>) (Some x) (Some y) = Some $ x <> y
  (<>) None _ = None
  (<>) _ None = None

instance Monoid a => Monoid (Option a) where
  mempty = None
  mappend = (<>)

sums :: Option (Sum Int)
sums =
  let
    x = Some 4
    y = Some 5
  in
    x <> y

sumsMempty :: Option (Sum Int)
sumsMempty =
  let
    x = Some 4
  in
    x <> mempty

prods :: Option (Product Int)
prods =
  let
    x = Some 4
    y = Some 5
  in
    x <> y

prodsMempty :: Option (Product Int)
prodsMempty =
  let
    x = Some 4
    y = Some mempty
  in
    x <> y

data Two a b = Two a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two m n) (Two p q) = Two (m <> p) (n <> q)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)


twoDemo :: Two (Sum Int) [Int]
twoDemo =
  let
    x = Two 5 [1,2]
    y = Two 3 [3,4]
  in
    x `mappend` y `mappend` mempty
