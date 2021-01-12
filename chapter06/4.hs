-- calculates the gcd of two non-negative integers using euclid's algorithm
euclid :: Int -> Int -> Int
euclid n m = if n == m then n else
             if n > m then euclid (n - m) m 
             else euclid n (m - n)
