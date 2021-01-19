import Data.Char

let2int :: Char -> Int
let2int c = if isLower c then ord c - ord 'a'
            else ord c - ord 'A'

int2letLow :: Int -> Char
int2letLow n = chr (ord 'a' + n)

int2letUp :: Int -> Char
int2letUp  n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2letLow ((let2int c + n) `mod` 26)
          | isUpper c = int2letUp  ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n xs = encode (-n) xs

-- produces frequency table of the english alphabet
table :: [Float]
table = [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,
         0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,
         6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

uppers :: String -> Int
uppers xs = length [x | x <- xs, x >= 'A' && x <= 'Z']

letters :: String -> Int
letters xs = lowers xs + uppers xs

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- produces a frequency table for a given string
freqs :: String -> [Float] 
freqs xs = [percent ((count low xs) + (count up xs)) n | (low,up) <- 
              zip ['a'..'z'] ['A'..'Z']]
           where n = letters xs

-- computes the chi-square statistics
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- rotates the elements of a list by a number smaller than the length of the list
-- with wrapping around
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- returns all positions of element a in a list
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

crack :: String -> String
crack xs = decode factor xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs
