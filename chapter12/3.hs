instance Functor ((->) a) where
   -- fmap :: (b -> c) -> (a -> b) -> (a -> c) 
   fmap = (.)

instance Applicative ((->) a) where
   -- pure :: b -> (a -> b)
   pure = const

   -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
   f <*> g = \x -> g x (f x)
