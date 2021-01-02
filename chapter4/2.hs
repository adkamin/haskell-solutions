-- returns the third element in a list that contains at least 3 elements, using head and tail
third1 :: [a] -> a
third1 xs = head (tail (tail xs))

-- returns the third element in a list that contains at least 3 elements, using list indexing !!
third2 :: [a] -> a
third2 xs = xs !! 2

-- returns the third element in a list that contains at least 3 elements, using pattern matching
third3 :: [a] -> a
third3 (_:_:a:_) = a

