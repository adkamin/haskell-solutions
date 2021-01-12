import Prelude hiding (sum, take, last)

-- calculates the sum of a list of numbers
sum :: Num a => [a] -> a
sum []     = 0
sum (n:ns) = n + sum ns

-- takes a given number of elements from the start of a list
take :: Int -> [a] -> [a]
take _ []     = []
take 0 xs     = []
take n (x:xs) = x : take (n-1) xs

-- selects the last element of a non-empty list
last :: [a] -> a
last [x]    = x
last (_:xs) = last xs
