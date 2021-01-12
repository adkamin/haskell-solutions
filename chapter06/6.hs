import Prelude hiding (and, concat, replicate, (!!), elem)

and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && (and xs)

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs 

replicate :: Int -> a -> [a]
replicate 0 x = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem e []     = False
elem e (x:xs) = if e == x then True else elem e xs



