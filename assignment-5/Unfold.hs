module Unfold where

import Data.List (unfoldr)
import Prelude hiding (take,zip,(++))

-- define all the below funtions using `unfoldr`
bits :: Int -> [Int]
bits = unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 2, x `div` 2))

zip :: [a] -> [b] -> [(a,b)]
zip xs ys = unfoldr (\(x:xs,y:ys) -> Just ((x,y), (xs,ys))) (xs,ys)

--take :: Int -> [a] -> [a]
primes :: [Integer]
primes = unfoldr (\(x:xs) -> Just (x, filter (\y -> y `mod` x /= 0) xs)) [2..]

-- alternative implementation of `primes`:
--primes' = sieve [2..]
--  where sieve (p:xs) = p : sieve [ n | n <- xs, n ‘mod‘ p /= 0 ]

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

-- (++) :: [a] -> [a] -> [a]
-- insert :: (Ord a) => a -> [a] -> [a]
-- unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
