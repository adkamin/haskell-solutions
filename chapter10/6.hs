import System.IO

{-
readLine :: IO String
readLine = do x <- getCh
              case x of
                 '\n'      -> do return []
                 '\DEL'    -> do putStr "\b \b"
                              xs <- readLine
                              return ('\DEL':cs)
                 otherwise -> do putChar x
                                 xs <- readLine
                                 return (x:xs)
                 
-}
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x
{-
reading :: IO ()
reading = do putStr ("Enter a word: " ++ show readLine)
-}
