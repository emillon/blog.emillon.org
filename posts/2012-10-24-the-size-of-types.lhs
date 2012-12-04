---
title: The size of types
author: Etienne Millon
tags: haskell
---

"Modern" programming languages have a very useful feature : type inference.
It makes the compiler compute the type of all expressions and variables without
the programmer having to write them in the program.

As every algorithm, type inference has a cost : if it was too expensive, it would
be prohibitive to use. Its complexity is actually exponential, but linear in
practice. In this article I'll present a classic case of exponential growth.

A size operator
===============

First, let's define the size of a type. Intuitively, the size of a type is the
number of type constants and constructors it contains :

> type T0 = (Int, Float -> () -> (Int, Int, Float))

Here, we have 6 type constants and 4 type constructor applications[^1], so size
of `t0` is 10.

[^1]: Here we have counted `()` as a type constant, but it can be equivalently
seen as a 0-ary type constructor.

Let's define a typeclass to compute this information.

> class Size a where
>    tsize :: a -> Int

For base types (`Int`, `Float`, `()`, etc), the size is (arbitrarly) 1.

> instance Size Int where
>    tsize _ = 1
>
> instance Size Float where
>    tsize _ = 1
>
> instance Size () where
>    tsize _ = 1

It is easy to extend `tsize` to tuples : the only thing to do is to add 1 to the
sum of size of all the arguments.

> instance (Size a, Size b) => Size (a, b) where
>   tsize (a, b) = 1 + tsize a + tsize b
>
> instance (Size a, Size b, Size c) => Size (a, b, c) where
>   tsize (a, b, c) = 1 + tsize a + tsize b + tsize c

For `a -> b`, it is trickier because we don't have to our disposition a value of
type `a`. A solution is to add an "oracle" to the context of `tsize`, to that
the instance will have a type `(NonEmpty a, ...) => Size (a -> b)`. Base types
are inhabited :

> class NonEmpty a where
>   some :: a
>
> instance NonEmpty () where
>   some = ()
>
> instance NonEmpty Int where
>   some = 0
>
> instance NonEmpty Float where
>   some = 0

Once again, the case of tuples is trivial.

> instance (NonEmpty a, NonEmpty b) => NonEmpty (a, b) where
>   some = (some, some)
>
> instance (NonEmpty a, NonEmpty b, NonEmpty c) => NonEmpty (a, b, c) where
>   some = (some, some, some)

Building a function is easy : by ignoring the argument of type `a`, we can
create a function of type `a -> b` provided that `b` is inhabited.

> instance NonEmpty b => NonEmpty (a -> b) where
>   some = const some

Finally, using this typeclass, we can write an instance for `Size (a -> b)` :
"magically" find a inhabitant for `a`, apply `f` to it and we have enough
information to sum the sizes of their types.

> instance (NonEmpty a, Size a, Size b) => Size (a -> b) where
>    tsize f =
>      let x = some in
>      1 + tsize x + tsize (f x)

Getting back to our example, we can verify that the size of `T0`…

> t0 :: Int
> t0 = tsize (some :: T0)

…is indeed 10.

An exponential combinator
=========================

We present here a simple combinator that applies a value twice to a binary
function :

> s :: a -> (a -> a -> b) -> b
> s x f = f x x

For example, `s 2 (+) == 4`. But by parenthesizing differently its type, we can
have a different look at it :

~~~~ {.haskell}
s :: a -> ((a -> a -> b) -> b)
~~~~

It transforms a type into another one with two occurrences :w

> x0 x = x :: ()
> x1 x = s x0 x :: ()
> x2 x = s x1 x :: ()
> x3 x = s x2 x :: ()
> x4 x = s x3 x :: ()
> x5 x = s x4 x :: ()
> x6 x = s x5 x :: ()
> x7 x = s x6 x :: ()

> s0 = tsize x0
> s1 = tsize x1
> s2 = tsize x2
> s3 = tsize x3
> s4 = tsize x4
> s5 = tsize x5
> s6 = tsize x6
> s7 = tsize x7

a_n = 2^(n+1) - 3

> s' x = (x, x)
> x0' = ()
> x1' = s' x0'
> x2' = s' x1'
> x3' = s' x2'
> x4' = s' x3'
> x5' = s' x4'
> x6' = s' x5'
> x7' = s' x6'

> s0' = tsize x0'
> s1' = tsize x1'
> s2' = tsize x2'
> s3' = tsize x3'
> s4' = tsize x4'
> s5' = tsize x5'
> s6' = tsize x6'
> s7' = tsize x7'
