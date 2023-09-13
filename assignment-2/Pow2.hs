module Pow2 where

pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)

-- Maximum n for Integer: 500000 still worked so probably really high/infinity depending on your CPU/Memory and patience
-- Maximum n for Int: 62, (63 gave a negative number and > 63 gives 0)
-- Maximum n for Float: 127, (128 gave infinity)
-- Maximum n for Double: 1023, (1025 gave infinity)