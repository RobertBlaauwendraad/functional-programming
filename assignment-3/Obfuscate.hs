module Obfuscate where

import Data.Char
import Data.List

randomList :: Int -> [Int]
randomList 1 = [1]
randomList 2 = [2, 1]
randomList 3 = [2, 1, 3]
randomList 4 = [3, 2, 4, 1]
randomList 5 = [3, 1, 4, 5, 2]
randomList 6 = [4, 1, 3, 5, 2, 6]
randomList 7 = [6, 5, 3, 1, 4, 2, 7]
randomList 8 = [5, 4, 2, 6, 3, 7, 1, 8]
randomList 9 = [8, 1, 5, 9, 7, 3, 2, 6, 4]
randomList 10 = [1, 3, 9, 5, 10, 2, 8, 7, 4, 6]
randomList 11 = [9, 11, 7, 5, 2, 10, 8, 1, 4, 3, 6]
randomList 12 = [5, 10, 1, 12, 8, 2, 11, 4, 6, 7, 9, 3]
randomList 13 = [9, 1, 2, 3, 7, 10, 6, 11, 8, 5, 12, 13, 4]
randomList 14 = [3, 14, 13, 9, 4, 1, 8, 5, 10, 6, 7, 2, 12, 11]
randomList 15 = [3, 1, 15, 2, 6, 9, 11, 5, 13, 7, 4, 8, 14, 10, 12]
randomList 16 = [16, 8, 9, 3, 1, 13, 2, 6, 7, 5, 10, 4, 15, 11, 14, 12]
randomList 17 = [5, 1, 7, 6, 11, 13, 4, 10, 12, 17, 16, 14, 2, 9, 8, 15, 3]
randomList 18 = [18, 4, 11, 1, 15, 8, 5, 16, 9, 7, 3, 13, 2, 6, 14, 12, 10, 17]
randomList 19 = [11, 8, 3, 9, 12, 17, 16, 19, 7, 15, 4, 14, 1, 5, 2, 6, 18, 10, 13]
randomList n = [1..n]

getMiddlePart :: [a] -> [a]
getMiddlePart [] = []
getMiddlePart [x] = []
getMiddlePart xs = tail (init xs)

shuffle :: String -> String
shuffle s = [x | (nn, x) <- sort [ (n, c) | (n, c) <- zip ( randomList (length s)) s ] ]

jumble :: String -> String
jumble xs | length xs == 1 = xs
          | length xs > 1 && ( last xs == '.' || last xs == ',' ||
            last xs == '!' || last xs == '?') = jumble ([head xs] ++ getMiddlePart xs) ++ [last xs]
          | otherwise = [head xs] ++ shuffle (getMiddlePart xs) ++ [last xs]

cambridge :: String -> String
cambridge s = unwords (map jumble (words s))
meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."
