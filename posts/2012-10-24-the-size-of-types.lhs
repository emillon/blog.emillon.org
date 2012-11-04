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
---------------

> class NonEmpty a where
>   some :: a
>
> instance NonEmpty () where
>   some = ()
>
> instance NonEmpty b => NonEmpty (a -> b) where
>   some = const some
> 
> class Size a where
>    tsize :: a -> Int
> 
> instance Size () where
>    tsize _ = 0
>
> instance (NonEmpty a, Size a, Size b) => Size (a -> b) where
>    tsize f =
>      let x = some in
>      1 + tsize x + tsize (f x)

> s f x = x f f

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
