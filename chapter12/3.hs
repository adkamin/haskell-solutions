instance Functor ((->) a) where
    -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap f a = (.)

instance Applicative ((->) a) where
    -- pure :: b -> (((->) a) b)
    -- pure :: b -> (a -> b)
    pure = const -- const is of type b -> a -> b, meaning of two arguments, the first is returned

    -- (<*>) :: ((->) a) (b -> c) -> (a -> b) -> (a -> c)
    -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
    f <*> g = \x -> f x (g x) -- given x, we return f x (g x)
    