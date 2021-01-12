-- decides if one list is chosen from another one
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice _ []      = False
isChoice xs (y:ys) = isChoice (removeFirst y xs) (ys)

-- removes first occurence of an element from a list
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst e []     = []
removeFirst e (x:xs) = if x /= e then x : removeFirst e xs
                       else xs
