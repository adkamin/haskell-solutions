import Prelude hiding (last)

-- returns the last element of a non-empty list
last :: [a] -> a
last xs = xs !! ((length xs) - 1)
