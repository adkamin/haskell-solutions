data Tree a = Leaf a | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs el (Leaf a)     = el == a
occurs el (Node l a r) = if compare el a == EQ then True
                         else if compare el a == LT then occurs el l
                         else occurs el r


