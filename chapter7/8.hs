import Data.Char

type Bit = Int

bin2intOld :: [Bit] -> Int
bin2intOld bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = make8 (n `mod` 2 : int2bin (n `div` 2))

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0) 

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = tail . id

addParity :: [Bit] -> [Bit]
addParity bits = if sum bits `mod` 2 == 0 then bits ++ [0]
                 else bits ++ [1]

checkParity :: [Bit] -> [Bit]
checkParity bits = if sum bits `mod` 2 == 0 then take ((length bits) - 1) bits
                   else error "wrong parity" 

transmit :: String -> String
transmit = decode . checkParity  . channel . addParity . encode
