---
title: Comonadic Life
author: Etienne Millon
tags: haskell, conway, life, comonad, algebra
---

This post is written in [Literate Haskell]. This means that you can copy it into 

> import Control.Comonad

> data Z1 a = Z1 [a] a [a]

> left1 :: Z1 a -> Z1 a
> left1 (Z1 (l:ls) x rs) = Z1 ls l (x:rs)
> left1 _ = error "left"

> right1 :: Z1 a -> Z1 a
> right1 (Z1 ls x (r:rs)) = Z1 (x:ls) r rs
> right1 _ = error "right"

> instance Functor Z1 where
>   fmap f (Z1 ls x rs) = Z1 (map f ls) (f x) (map f rs)

> iterate' :: (a -> a) -> a -> [a]
> iterate' f = tail . iterate f

> instance Extend Z1 where
>   duplicate z = Z1 (iterate' left1 z) z (iterate' right1 z)

> instance Comonad Z1 where
>   extract (Z1 _ x _) = x

> data Z a = Z { unZ :: Z1 (Z1 a) }

> left :: Z a -> Z a
> left (Z z) = Z (fmap left1 z)

> right :: Z a -> Z a
> right (Z z) = Z (fmap right1 z)

> up :: Z a -> Z a
> up (Z z) = Z (left1 z)

> down :: Z a -> Z a
> down (Z z) = Z (right1 z)

> instance Functor Z where
>   fmap f (Z z) = Z (fmap (fmap f) z)

> dupLR :: Z a -> Z1 (Z a)
> dupLR z =
>   Z1 (iterate' left z) z (iterate' right z)

> instance Extend Z where
>   duplicate z =
>     Z $ fmap dupLR $ Z1 (iterate' up z) z (iterate' down z)

> instance Comonad Z where
>   extract (Z z) = extract $ extract z

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

> disp1 :: Z1 Bool -> String
> disp1 z =
>   pp z dispC
>     where
>       dispC True  = '*'
>       dispC False = ' '

> disp :: Z Bool -> String
> disp (Z z) = 
>   unlines $ pp z disp1

> pp :: Z1 a -> (a -> b) -> [b]
> pp (Z1 ls x rs) f =
>   map f $ reverse (take n ls) ++ [x] ++ take n rs
>     where
>       n = 6

> glider :: Z Bool
> glider =
>   Z $ Z1 (repeat fz) fz rs
>     where
>       rs = [ line [f, t, f]
>            , line [f, f, t]
>            , line [t, t, t]
>            ] ++ repeat fz
>       t = True
>       f = False
> 
>       fl = repeat f
>       fz = Z1 fl f fl
> 
>       line l =
>         Z1 fl f (l ++ fl)
