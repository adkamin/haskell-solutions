-- returns the sum of the non-negative integers from a given value down to 0
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)
