-- returns the list of triples (a,b,c) which satisfy a^2 + b^2 = c^2, up to a given limit
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]
