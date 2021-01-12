-- returns a coordinate grid of size m x n
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]
