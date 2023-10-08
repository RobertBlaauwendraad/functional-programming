module FunList where

--define using the _list design pattern_
compose :: [a -> a] -> (a -> a)
compose [] = id
compose (f:fs) = f . compose fs

--define using `foldr`
compose' :: [a -> a] -> (a -> a)
compose' = foldr (.) id

--Explain _what_ the following function computes, and _how_ it computes it
{-

It computes the factorial by generating a list of functions that multiply the current number by the next number in the list.
It then composes these functions together, and applies the resulting function to 1, which is the identity element for multiplication.

-}
foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1

--define in terms of *only* `map` and `compose`
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f b = compose . map f