-- returns a list of factors of a number
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- returns a list of numbers whose sum of all its factors is equal to the number itself
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum (factors x) -x) == x]

