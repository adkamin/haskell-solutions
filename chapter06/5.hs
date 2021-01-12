import Prelude hiding (length, drop, init)

length ::[a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

{-
   length [1,2,3]
=     { applying length }
   1 + length [2,3]
=     { applying length }
   1 + (1 + length [3])
=     { applying lenght }
   1 + (1 + (1 + length []))
=     { applying length }
   1 + (1 + (1 + 0))
=     { applying + }
   3
-}

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

{-
   drop 3 [1,2,3,4,5]
=     { applying drop }
   drop 2 [2,3,4,5]
      { applying drop }
   drop 1 [3,4,5]
      { applying drop }
   drop 0 [4,5]
      { applying drop }
   [4,5]
-}

init :: [a] -> [a]
init [_] = []
init (x:xs) = x : init xs

{-
   init [1,2,3]
=     { applying init }
   1 : init [2,3]
=     { applying init }
   1 : (2 : init [3])
      { applying init }
   1 : (2 : ([]))
      { list notation }
   [1,2]
-}
