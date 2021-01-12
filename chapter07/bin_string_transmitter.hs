import Data.Char

-- this binary to string transmitter assumes that the binary
-- number is written in reverse order, for example 13 = 1011
type Bit = Int

bin2intOld :: [Bit] -> Int
bin2intOld bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1

-- iterate produces an infinite list by applying a function an increasing
-- number of times to a value:
-- iterate f x = [x, f x, f (f x), f (f (f x)), ...]

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

-- we make use of the fact that:
-- (1*2) + (2*b) + (4*c) + (4*d) = a + 2*(b + 2*(c + 2*(d + 2*0)))

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = make8 (n `mod` 2 : int2bin (n `div` 2))

-- because:
-- 13 div 2 = 6, remainder [1]
-- 6  div 2 = 3, remainder [0]
-- 3  div 2 = 1, remainder [1]
-- 1  div 2 = 0, remainder [1] -> 13 in binary is 1011

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0) 

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- decoding works by chopping the array into 8-bit letters and converting each letter
-- into a unicode character

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode
