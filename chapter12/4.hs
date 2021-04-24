newtype ZipList a = Z [a]
    deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap f (Z xs)  = Z (fmap f xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList [a]
    pure a = Z (repeat a)

    -- <*> :: ZipList (a -> b) -> (ZipList a) -> (ZipList b)
    (Z fs) <*>  (Z xs) = Z [g x | (g,x) zip gs xs]