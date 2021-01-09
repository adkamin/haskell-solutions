data Tree a = Leaf a | Node (Tree a) (Tree a)

-- balanced
t1 :: Tree Int
t1 = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9))

-- imbalanced
t2 :: Tree Int
t2 = Node (Node (Node (Leaf 1) (Leaf 4)) (Leaf 3)) (Leaf 1) 

balanced :: Tree a -> Bool
balanced (Leaf a)   = True
balanced (Node l r) = left == right || left+1 == right || left == right+1
                      where
                         left  = countLeaves l
                         right = countLeaves r

countLeaves :: Tree a -> Int
countLeaves (Leaf a)   = 1
countLeaves (Node l r) = countLeaves l + countLeaves r
