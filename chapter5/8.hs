-- returns all positions of element a in a list
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- positions implemented using find
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positionsNew :: Eq a => a -> [a] -> [Int]
positionsNew x xs = find x (zip xs [0..])
