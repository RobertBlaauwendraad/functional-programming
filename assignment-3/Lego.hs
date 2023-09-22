module Lego where

import Data.List
import Data.Tuple

removeAt :: Int -> [a] -> [a]
removeAt n xs = [ x | (i,x) <- zip [0..] xs, i /= n ]

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos xs = sort [(x,i)| (x,i) <- zip xs [0..]]

sortedPos :: (Ord a) => [a] -> [(a,Int)]
sortedPos xs = [(x,j)|((i,x),j) <- sort[((i,x),j)| ((x,i),j) <- zip (sortWithPos xs) [0..]]]
