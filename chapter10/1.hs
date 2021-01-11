import Prelude hiding (putStr)

{-
putStr :: String -> IO()
putStr []     = return ()
putStr (x:xs) = do putChar x
                   putStr xs
-}

putStr :: String -> IO()
putStr str = sequence_  [putChar s | s <- str]

