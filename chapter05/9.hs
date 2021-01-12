-- computes the scalar product of two lists
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]
--
scalar :: [Int] -> [Int] -> Int
scalar xs ys = sum [x*y | (x,y) <- zip xs ys]
