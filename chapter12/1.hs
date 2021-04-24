data Tree a = Leaf | Node (Tree a) a (Tree a)
            deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = Leaf
    fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)