-- If <= would be replaced by <, duplicates would disappear.
-- qsort [2,2,3,1,1] then produces the list [1,2,3]

-- a qsort producing a sorted version of a list, without duplicates
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                  smaller = [a | a <- xs, a < x]
                  larger  = [b | b <- xs, b > x]

