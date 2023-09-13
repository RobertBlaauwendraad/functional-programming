module Char where

import Data.Char

(~~) :: String -> String -> Bool
(~~) n m = map toLower n == map toLower m

reverseCase :: String -> String
reverseCase = map (\c -> if isUpper c then toLower c else toUpper c)

shift :: Int -> Char -> Char
shift n c
  | isUpper c && isAscii c = chr $ ord 'A' + (ord c - ord 'A' + n) `mod` 26
  | otherwise = c

caesar :: Int -> String -> String
caesar n s = map (shift n) (map toUpper s)

msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"
-- caesar 5 msg gives:
-- "FIRST I MUST SPRINKLE YOU WITH FAIRY DUST"