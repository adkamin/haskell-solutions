import Data.Char

getInts :: Int -> IO [Int]
getInts max | max == 0  = return []
            | otherwise = do n  <- getInt
                             ns <- getInts (max-1)
                             return (n:ns)

getInt :: IO Int
getInt = do x <- getLine
            return (read x :: Int) 

adder :: IO ()
adder = do putStr "How many numbers? "
           n  <- getInt
           ns <- getInts n
           putStr "The total is "
           print (sum ns)
           return ()
