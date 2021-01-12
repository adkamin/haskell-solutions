import Prelude hiding ((||))
{-
(||) :: Bool -> Bool -> Bool
True || True   = True
True || False  = True
False || True  = True
False || False = False


(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True


(||) :: Bool -> Bool -> Bool
False || b     = b
b     || False = b
True  || _     = True
-}

(||) ::  Bool -> Bool -> Bool
b || c | b /= c    = True
       | b == True = True
       | otherwise = False
