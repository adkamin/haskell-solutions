-- merges two sorted lists into a final sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs         = xs
merge xs []         = xs
merge (x:xs) (y:ys) = smallest : mergerest
                        where 
                          smallest = min x y
                          mergerest = if (smallest == x) then merge xs (y:ys)
                                      else merge (x:xs) ys
