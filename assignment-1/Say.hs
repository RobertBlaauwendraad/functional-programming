module Say where

say :: Integer -> String
say  0 = "zero"
say  1 = "one"
say  2 = "two"
say  3 = "three"
say  4 = "four"
say  5 = "five"
say  6 = "six"
say  7 = "seven"
say  8 = "eight"
say  9 = "nine"
say 10 = "ten"
say 11 = "eleven"
say 12 = "twelve"
say 13 = "thirteen"
say 14 = "fourteen"
say 15 = "fifteen"
say 16 = "sixteen"
say 17 = "seventeen"
say 18 = "eighteen"
say 19 = "nineteen"
say 20 = "twenty"
say 30 = "thirty"
say 40 = "forty"
say 50 = "fifty"
say 60 = "sixty"
say 70 = "seventy"
say 80 = "eighty"
say 90 = "ninety"
say n = calculateByOOM n (orderOfMagnitude n)
  where
    calculateByOOM :: Integer -> Integer -> String
    calculateByOOM n m
      | m >= 4 = say (getThousands n) ++ " thousand" ++ (if n - getThousands n * 10^3 /= 0 then " " ++ say (n - (getThousands n * 10^3)) else "")
      | m == 3 = say (firstDigit n m) ++ " hundred" ++ (if (n - addZeros (firstDigit n m) m) /= 0 then " " ++ say (n - addZeros (firstDigit n m) m) else "")
      | m == 2 = say (addZeros (firstDigit n m) m) ++ " " ++ say (n - addZeros (firstDigit n m) m)
    firstDigit :: Integer -> Integer -> Integer
    firstDigit n m = n `div` 10^(m-1)
    orderOfMagnitude :: Integer -> Integer
    orderOfMagnitude n = floor (logBase 10 (fromIntegral n) + 1)
    addZeros :: Integer -> Integer -> Integer
    addZeros n m = n * 10^(m-1)
    getThousands :: Integer -> Integer
    getThousands n = n `div` 10^3