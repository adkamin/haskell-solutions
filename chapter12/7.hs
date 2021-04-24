data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Var a)   = Var (f a)
    fmap _ (Val i)   = Val i
    fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
    -- pure :: a -> f a
    pure = Var

    -- (<*>) :: f (a -> b) -> f a -> f b
    (Var a) <*> f   = Var (fmap f a)
    (Val i) <*> _   = Val i
    (Add l r) <*> f = Add (l <*> f) (r <*> f)

instance Monad Expr where
    -- return :: a -> m a
    return = pure

    -- (>>=) :: m a -> (a -> m b) -> m b
    (Var a) >>= f   = f a
    (Val i) >>= _   = Val i
    (Add l r) >>= f = Add (l >>= f) (r >>= f)
