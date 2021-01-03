-- divides the list into halves whose lengths differ by at most one
halve :: [a] -> ([a],[a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

-- merges two sorted lists into a final sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs         = xs
merge xs []         = xs
merge (x:xs) (y:ys) = smallest : mergerest
                        where 
                          smallest = min x y
                          mergerest = if (smallest == x) then merge xs (y:ys)
                                      else merge (x:xs) ys

-- sorts the list using merge sort
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x] 
msort xs  = merge (msort left) (msort right)
              where (left,right) = halve xs
