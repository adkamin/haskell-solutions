-- type of control stacks for the abstract machine, which comprise a list of operations
-- to be performed by the machine after the current evaluation has been completed
type Cont = [Op]

data Op = EVAL Expr | ADD Int

data Expr = Val Int | Add Expr Expr

-- evaluates an expression in the context of a control stack
eval :: Expr -> Cont -> Int
eval (Val n) c    = exec c n
eval (Add x y) c  = eval x (EVAL y : c)

-- executes a control stack in the context of an integer argument
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m  = exec c (n + m) 

-- evaluates an expression to an integer, by invoking eval with the given expression
-- and the empty stack
value :: Expr -> Int
value e = eval e []
