-- returns the tail of a list if the list is non-empty, returns an empty list otherwise - using conditional expression
safetail1 :: [a] -> [a]
safetail1 xs = if length xs == 0 then [] else tail xs

-- returns the tail of a list if the list is non-empty, returns an empty list otherwise - using guarded equations
safetail2 :: [a] -> [a]
safetail2 xs | length xs == 0 = []
             | otherwise      = tail xs

-- returns the tail of a list if the list is non-empty, returns an empty list otherwise - using pattern matching
safetail3 :: [a] -> [a]
safetail3 []    = [] 
safetail3 (_:a) = a


