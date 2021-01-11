import System.IO

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

-- reads word and turns each character as a dash
sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else 
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

-- reads a character without echoing 
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

-- main game loop
play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                  putStrLn "You got it!"
               else 
                  do putStrLn (match word guess)
                     play word

-- indicates which letters in the secret word occur anywhere in the guess
match :: String -> String -> String
match s g = [if elem c g then c else '-' | c <- s]
                  


