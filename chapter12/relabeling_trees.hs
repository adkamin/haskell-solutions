data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- passing the integer (for relabeling) as a state 
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n   = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
                      where
                         (l',n')  = rlabel l n
                         (r',n'') = rlabel r n' 

type State   = Int
newtype ST a = S (State -> (a,State))

instance Functor ST where
   -- fmap :: (a -> b) -> ST a -> ST b
   fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
   -- pure :: a -> ST a
   pure x = S (\s -> (x,s))

   -- (<*> :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = S (\s -> let (f,s')  = app stf s
                              (x,s'') = app stx s' in (f x, s''))      

instance Monad ST where
   -- (>>=) :: ST a -> (a -> ST b) -> ST b
   st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

app :: ST a -> State -> (a,State)
app (S st) x = st x

fresh :: ST Int
fresh = S (\n -> (n, n+1))

-- using the state monad for relabeling (applicative style)
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r 

-- using the do-notation (monadic style)
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _)   = do n <- fresh
                       return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')
