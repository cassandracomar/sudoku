module Data.Utils where

{- | flipped double composition. applies the one-argument function on the right to the result of the two-argument function
on the left after both arguments are applied to the function on the left.

i.e. `f >>>> g = \x y -> g (f x y)`
-}
(>>>>) :: (a1 -> a2 -> b) -> (b -> c) -> a1 -> a2 -> c
(>>>>) = flip $ (.) . (.)
{-# INLINE (>>>>) #-}

infixr 1 >>>>
