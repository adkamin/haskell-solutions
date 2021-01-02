-- returns the sum of the first 100 integer squared
sumSquared :: Int
sumSquared = sum [n^2 | n <- [1..100]]
