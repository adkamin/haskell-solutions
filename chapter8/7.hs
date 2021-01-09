import Prelude hiding (Eq, (==))

class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)

instance Eq Bool where
   False == False = True
   True  == True  = True
   _     == _     = False

instance Eq a => Eq (Maybe a) where
   Nothing == Nothing = True
   Just x  == Just y  = x == y
   _       == _        = False

instance Eq a => Eq [a] where
   (x:xs)  == (y:ys)  = x == y && xs == ys
   (x:xs)  == []      = False
   []      == (x:xs)  = False
   []      == []      = True
   



