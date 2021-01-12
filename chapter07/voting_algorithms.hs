import Data.List

-- first pass the post voting: each person has one vote and the candidate with
-- the largest number of votes is the winner
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- counts the number of occurences of an element in the list
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- removes duplicates from the list
rmdups :: Eq a => [a]  -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- produces a list of tuples of number of votes and cadidates in increasing order
result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

-- determines the winner
winner :: Ord a => [a] -> a
winner = snd . last . result


-- alternative voting: each person can list as many candidates as they want
-- listing them in preference order on their ballots
ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

-- eliminates the given candidate from the list of candidates
elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

-- removes empty ballots
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

-- ranks the 1st choice candidates in each ballot in increasing order of the nr of votes
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

-- determines the winner
winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c]    -> c
                (c:cs) -> winner' (elim c bs)

-- the winner is determined in the following way: first the empty ballots are removed
-- and the remaining ballots are sorted. If only one such ballot is left, it is the
-- winner. Otherwise, we remove the one with the smallest amount of votes and repeat
 

