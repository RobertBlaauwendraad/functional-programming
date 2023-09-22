module Uniq where

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x:y:xs) = if x == y then uniq (y:xs) else x : uniq (y:xs)