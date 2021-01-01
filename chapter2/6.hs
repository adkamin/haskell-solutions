import Prelude hiding (init)

-- removes the last element of a list
init1 :: [a] -> [a]
init1 [] = []
init1 xs = take ((length xs) - 1) xs

-- removes the last element of a list
init2 :: [a] -> [a]
init2 [] = []
init2 xs = reverse (tail (reverse xs))
