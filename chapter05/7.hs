-- a list comprehension with two generators
compr :: [(Int,Int)]
compr = [(x,y) | x <- [1,2], y <- [3,4]]

-- two comprehensions with single generators
comprNew :: [(Int,Int)]
comprNew = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]
