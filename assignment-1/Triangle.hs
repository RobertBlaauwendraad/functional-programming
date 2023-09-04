triangle :: Integer -> String
triangle n = unlines [replicate (fromIntegral n - fromIntegral i) ' ' ++ replicate (2 * fromIntegral i + 1) '*' | i <- [0..n-1]]
