import Prelude hiding (length, replicate)

length :: [a] -> Int
length xs = sum [1 | _ <- xs]

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]
