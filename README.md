Algebras
--------

### Writing Typeclasses

To write a typeclass instance for a datatype, run `:i <Typeclass>` to
find its kindedness:

``` {.haskell}
class Semigroup a where
class Functor (f :: * -> *) where
```

Semigroup expects a concrete type constructor whereas functor expects
one with a type parameter. If I\'m writing a semigroup instance for
`Maybe`, a type constructor of kind `* -> *`, I must supply the type
parameter in the instance header. And I might have to constrain the
parameter, depending on what I want to do with it.

Once I have the header done, it\'s just a matter of defining the
required functions for all possible combinations of data constructors.

### General Pattern

At Haskell\'s core are several mathematical structures. They are defined
via typeclass. They are, in order of increasing strength:

-   semigroup
-   monoid
-   functor
-   applicative
-   monad

### Semigroup

About combination--going from two things to one thing. Note some
datatypes have several semigroup instances because they can be combined
in multiple ways. Int for instance. Should \"5 combine 4\" be 9 or 20?

### Monoid

Same as semigroup only with an identity element called `mempty` (so for
list it\'s `[]`, Sum Int 0, Product Int 1, etc). Monoid requires
semigroup, meaning all monoids are semigroups but not all semigroups are
monoids.

### Functor

Functors are structures with some data inside (hence the `* -> *`
kindedness). The idea behind functor is to lift some function over that
structure and apply it do the data inside, leaving the structure itself
intact.

### Applicative

Like function only instead of having a function and some structure, you
have two structures. One of these has a function inside it, so the idea
is to apply the function and combine the two structures.

For types like Maybe, you can think of this as extracting the function
from one structure, extracting the data from another, applying one to
the other, and wrapping it back in a Maybe structure. But for something
like tuple, the first element in the tuple is part of the structure, so
after you apply the function, you\'re still left with the problem of how
to combine this element with its counterpart.

`(<*>)` is read \"ap\" or \"apply\"

### Monad

The idea behind monad resembles that behind applicative. It\'s about
applying a function to a datum inside some structure, and then possibly
doing some combining (depending on the structure). The difference is
that with applicative, you\'re given two structures up front. With
monad, you\'re given a structure and a function that returns a
structure. So you must apply the function then (potentially) combine
it\'s result with the original structure. (Compare applicative and monad
implementations of the Two--ie, tuple--datatype).

Function Algebras
-----------------

### General

Functions too can have a functor instance, a monad instance, etc. It may
seem strange, but if you think of the function `a -> b` as a structure
`(a ->)` with a type parameter `b`, then it becomes no different than
the structure `Maybe b` or `List b`. They key to writing typeclass
instances for functions is to look at the typeclass method signatures
and the substitute in the function structure. For example:

``` {.haskell}
fmap :: Functor f => (a -> b) -> f a -> f b
```

Let `f` be `(r -> )`:

``` {.haskell}
fmap :: Functor f => (a -> b) -> (r -> a) -> (r -> b)
```

And then it\'s simply a matter of lining up the types.

The interesting thing about this idea is that defining monad instances
for certain functions (`a -> b` or `a -> (b, a)` for example) gives us
new abilities in our code.

### Reader

Reader is just the function `a -> b`, or more conventionally `r -> s`.
It\'s monad instance is useful if we have a bunch of functions that need
to operate on--but not change--some global state. It\'s tedious to pass
the global state around to all the functions so reader let\'s us avoid
that.

### State

If we need to modify state, not just read it, then the State monad is
the thing. It is an instance of the funciton `s -> (a, s)`.

Transformers
------------

combining monadic effects with composition. State lets you modify values
over time. IO lets you print. Combine them to do both. You can do this
without transformers but there\'s a lot of tedious packaging/unpackaging
values.

can only compose monads if you know one of them in advance, hence the
need for transformers

building a little onion. M1 M2 means M1(M2). To evaluate you must peelM1
and THEN peelM2
