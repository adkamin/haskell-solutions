import Prelude hiding (all, any, takeWhile, dropWhile)

-- returns true if all elements satisfy the predicate
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- returns true if any element satisfies the predicate
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- selects elements from the list while they satisfy a predicate
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs
                     else []
-- removes elements from the list while they satisfy a predicate
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile p (x:xs) = if p x then dropWhile p xs
                     else x:xs
                    
