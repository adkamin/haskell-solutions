import Prelude hiding (map, iterate)
import Data.List

type Bit = Int

unfold :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2binOld :: Int -> [Bit]
int2binOld 0 = []
int2binOld n = n `mod` 2 : int2bin (n `div` 2)

int2bin :: Int -> [Bit]
int2bin = unfold (==0) (`mod` 2) (`div` 2)

chop8old :: [Bit] -> [[Bit]]
chop8old []   = []
chop8old bits = take 8 bits : chop8 (drop 8 bits)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (==[]) (take 8) (drop 8)

mapOld :: (a -> b) -> [a] -> [b]
mapOld f []     = []
mapOld f (x:xs) = f x : mapOld f xs

--map :: (a -> b) -> [a] -> [b]
--map f = unfold (==[]) (f (head)) (tail)

iterateOld :: (a -> a) -> a -> [a]
iterateOld f a = a : iterateOld f (f a)

--iterate :: (a -> a) -> a -> [a]
--iterate f a = unfold (==[]) (id . head) (f . tail)
