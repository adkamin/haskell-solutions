import Prelude hiding (last)

-- returns the last element of the list
last :: [a] -> Maybe a
last [] = Nothing
last xs = Just (xs !! ((length xs) - 1))
