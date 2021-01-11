import Data.Char

getInts :: Int -> IO [Int]
getInts max = sequence (replicate max getInt)

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
