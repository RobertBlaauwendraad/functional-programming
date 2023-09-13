module PolyType where

f8 x y  = if x <= y then x else y

f9 x y  = not x || y

f10 x y
  | x == 0    = y
  | otherwise = x + y

f11 x y = get 0
  where get n = if n == 0 then x else y

-- a)
-- f8 and f11 can be used on arguments of type String
-- b)
-- f8 is ad-hoc polymorphic, the type is of the form Ord a => a -> a -> a
-- f9 is not polymorphic, the type is of the form Bool -> Bool -> Bool
-- f10 is ad-hoc polymorphic, the type is of the form Num a => a -> a -> a
-- f11 is parametric polymorphic, the type is of the form a -> a -> a