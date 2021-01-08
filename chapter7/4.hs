dec2int :: [Int] -> Int
dec2int = foldl (\n ns -> 10*n + ns) 0

-- > dec2int [2,3,4,5] 
-- 2345
