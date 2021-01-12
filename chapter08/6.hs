data Expr = Val Int | Add Expr Expr

ex1 :: Expr
ex1 = (Add (Add (Val 2) (Val 3)) (Val 4))

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val i)     = f i
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

eval :: Expr -> Int
eval e = folde id (+) e

size :: Expr -> Int
size (Val i)     = 1
size (Add e1 e2) = size e1 + size e2
