---
title: Comonadic Life
author: Etienne Millon
tags: haskell, conway, life, comonad, algebra
---

This post is written in [Literate Haskell]. This means that you can copy it into
a `.lhs` file[^1] and run it through a Haskell compiler or interpreter.

[^1]: I should make the source directly available too.

Today we'll talk about...

> import Control.Comonad

Comonads ! They are the categoric dual of monads, which means that the type
signatures of comonadic functions look like monadic functions, but with the
arrow reversed. I am not an expert in category theory, so I won't go further.

I will use the following typeclass for comonads : it's from [Edward Kmett]'s
[comonad package] (split from the infamous [category-extras package]).

~~~~ {.haskell}
class Functor w => Comonad w where
  extract :: w a -> a
  extend :: (w a -> b) -> w a -> w b
  duplicate :: w a -> w (w a)
~~~~

The Monad typeclass, for reference, can be described as[^2] :

[^2]: In the real Haskell typeclass, there are the following differences: Monad
      and Functor are not related, `join` is a library function (you can't use
      it to define an instance), `(>>=)` is used instead of its flipped
      counterpart `(=<<)` and there two more methods `(>>)` and `fail`.

~~~~ {.haskell}
class Functor m => Monad m where
  return :: a -> m a
  (=<<) :: (a -> m b) -> m a -> m b
  join :: m (m a) -> m a
~~~~

The duality is quite easy to see : `extract` is the dual of `return`, `extend`
the one of `(=<<)` and `duplicate` the one of `join`.

So what are comonads good for ?

I stumbled upon [an article][sigfpe article] which explains that they can be
used for computations which depend on some local environment, like [cellular
automata]. A comment asks whether it's possible to generalize to higher
dimensions, which I will do by implementing [Conway's Game of Life] in a
comonadic way.

List Zippers
------------

List zippers are a fantastic data structure, allowing O(1) edits at a "cursor".
Moving the cursor element to element is O(1) too. This makes it a very nice data
structure when your edits are local (say, in a text editor). You can learn more
about zippers in general in this [post from Edward Z Yang]. The seminal paper is
of course [Huet's article].

A list zipper is composed of a cursor and two lists.

> data ListZipper a = LZ [a] a [a]

To go in a direction, you pick the head of a list, set it as your cursor, and
push the cursor on top of the other list. We assume that we will only infinte
lists, so this operation can not fail. This assumption is reasonnable especially
in the context of cellular automata[^3].

[^3]: Simulating a closed topology such as a torus may even be possible using
      cyclic lists instead of lazy infinite lists.

> listLeft :: ListZipper a -> ListZipper a
> listLeft (LZ (l:ls) x rs) = LZ ls l (x:rs)
> listLeft _ = error "listLeft"
>
> listRight :: ListZipper a -> ListZipper a
> listRight (LZ ls x (r:rs)) = LZ (x:ls) r rs
> listRight _ = error "listRight"

Reading and writing on a list zipper at the cursor is straightforward :

> listRead :: ListZipper a -> a
> listRead (LZ _ x _) = x
>
> listWrite :: a -> ListZipper a -> ListZipper a
> listWrite x (LZ ls _ rs) = LZ ls x rs

We can easily define a `Functor` instance for `ListZipper` : to apply a function
on whole zipper, we apply it to the cursor and map it on the two lists :

> instance Functor ListZipper where
>   fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

Time for the `Comonad` instance. The, `extract`, returns an element from the
structure : we can pick the one at the cursor.

`duplicate` is a bit harder to grasp. From a list zipper, we have to build a
list zipper of list zippers. The signification behind this (confirmed by the
comonad laws that every instance has to fulfill) is that moving inside the
duplicated structure returns the original structure, disformed by the same
move.

This means that at the cursor of the duplicated structure, there is the original
structure `z`. And the left list is composed of `listLeft z`, `listLeft
(listLeft z)`, `listLeft (listLeft (listLeft z))`, etc (same goes for the right
list).

> instance Comonad ListZipper where
>   extract = listRead
>
>   duplicate z = LZ (iterate' listLeft z) z (iterate' listRight z)

`iterate'` is a small helper which maps to x, `[f x, f (f x), ...]`.

> iterate' :: (a -> a) -> a -> [a]
> iterate' f = tail . iterate f

Plane zippers
-------------

Let's generalize list zippers to plane zippers, which are cursors on a plane
of cells. We will implement them using a list zipper of list zippers.

> data Z a = Z { unZ :: ListZipper (ListZipper a) }

> left :: Z a -> Z a
> left (Z z) = Z (fmap listLeft z)

> right :: Z a -> Z a
> right (Z z) = Z (fmap listRight z)

> up :: Z a -> Z a
> up (Z z) = Z (listLeft z)

> down :: Z a -> Z a
> down (Z z) = Z (listRight z)

> instance Functor Z where
>   fmap f (Z z) = Z (fmap (fmap f) z)

> dupLR :: Z a -> ListZipper (Z a)
> dupLR z =
>   LZ (iterate' left z) z (iterate' right z)

> instance Comonad Z where
>   extract (Z z) = extract $ extract z
>
>   duplicate z =
>     Z $ fmap dupLR $ LZ (iterate' up z) z (iterate' down z)

> ctx :: Z Bool -> Int
> ctx z =
>   length $ filter (==True) cells
>     where
>       cells = map (\d -> extract (d z)) dirs
>       dirs = [ up . left
>              , up
>              , up . right
>              , left
>              , right
>              , down . left
>              , down
>              , down . right
>              ]

> rule :: Z Bool -> Bool
> rule z =
>   case ctx z of
>     2 -> extract z
>     3 -> True
>     _ -> False

> evolve :: Z Bool -> Z Bool
> evolve = extend rule

> disp1 :: ListZipper Bool -> String
> disp1 z =
>   pp z dispC
>     where
>       dispC True  = '*'
>       dispC False = ' '

> disp :: Z Bool -> String
> disp (Z z) =
>   unlines $ pp z disp1

> pp :: ListZipper a -> (a -> b) -> [b]
> pp (LZ ls x rs) f =
>   map f $ reverse (take n ls) ++ [x] ++ take n rs
>     where
>       n = 6

> glider :: Z Bool
> glider =
>   Z $ LZ (repeat fz) fz rs
>     where
>       rs = [ line [f, t, f]
>            , line [f, f, t]
>            , line [t, t, t]
>            ] ++ repeat fz
>       t = True
>       f = False
>
>       fl = repeat f
>       fz = LZ fl f fl
>
>       line l =
>         LZ fl f (l ++ fl)

[Literate Haskell]:          http://www.haskell.org/haskellwiki/Literate_programming
[Edward Kmett]:              http://comonad.com
[comonad package]:           http://hackage.haskell.org/packages/archive/comonad/3.0.0.2/doc/html/Control-Comonad.html
[category-extras package]:   http://hackage.haskell.org/package/category-extras-1.0.2
[sigfpe article]:            http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
[post from Edward Z Yang]:   http://blog.ezyang.com/2010/04/you-could-have-invented-zippers/
[Huet's article]:            http://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
