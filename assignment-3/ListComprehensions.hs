module ListComprehensions where

g0 :: [a] -> [b] -> [(a,b)]
g0 as bs = [ (a,b) | a <- as, b <- bs ]
-- cartesianProduct, combines every element of as with every element of bs
-- Polymorphic

g1 :: Int -> a -> [a]
g1 n y   = [ y | i <- [1..n] ]
-- replicate, creates a list of n elements, all equal to y
-- Polymorphic and overloaded function

g2 :: Int -> [a] -> [a]
g2 n xs  = [ x | (i,x) <- zip [0..] xs, i < n ]
-- take, takes the first n elements of xs
-- Polymorphic and overloaded function

g3 :: (Eq a) => a -> [a] -> [Int]
g3 a xs  = [ i | (i,x) <- zip [0..] xs, x == a]
-- findIndices, finds the indices of all elements in xs that are equal to a
-- Overloaded function

g4 :: [a] -> [a] -> [a]
g4 xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]
-- interleave, interleaves the elements of xs and ys
-- Polymorphic function

g5 :: [[a]] -> [a]
g5 xss   = [ x | xs <- xss, x <- xs ]
-- concat, concatenates the elements of xss
-- Polymorphic function
