-- Countdown problem: Given a sequence of numbers and a target number, attempt to 
-- construct an expression whose value is the target by combining on or more of those
-- numbers and the addition, subtraction, multiplication, division and parenthesis

-----------------------------------Arithmetic operators----------------------------------
data Op = Add | Sub | Mul | Div

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"

-- returns true if the application of an operaton to two positive naturals gives another
-- positive natural
{-
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
-}

-- applies an operator to two positive naturals, returns another positive natural 
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

---------------------------------Numeric expressions------------------------------------
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
   show (Val n)     = show n
   show (App o l r) = brak l ++ show o ++ brak r
                      where
                         brak (Val n) = show n
                         brak e       = "(" ++ show e ++ ")"

e :: Expr 
e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)) 

-- returns the list of values in an expression
values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

-- returns the overall value of an expression
eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

---------------------------------Combinatorial functions---------------------------------
-- returns all subsequences of a list, which are given by all possible coombinations 
-- of including or excluding each element of the list
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

-- returns all possible ways of inserting an element into a list
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys) 

-- returns all permutations of a list
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- returns all possible ways of selecting 0 or more elements in any order
choices :: [a] -> [[a]]
choices = concat . map perms . subs

----------------------------Formalizing the problem--------------------------------------
-- returns true is an instance of the countdown problem has been solved
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-------------------------------Brute force solution--------------------------------------
-- returns all possible ways of splitting a list into two non-empty lists that append
-- to give the original list
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- returns all possible expressions whose list of values is precisely a given list
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

-- combines a pair of expressions using each of the four numberic operators
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

-- returns all possible expressions that solve an instance of the countdown problem
-- by first generating all expressions over each choice from the given list of numbers
-- then selecting those expressions that successully evaluate to give the target
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-------------------------------Performance testing---------------------------------------
main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)

-----------------------Combining generation and evaluation-------------------------------
type Result = (Expr,Int)

-- returns all possible results whose list of values is precisely a given list
results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns  = [res | (ls,rs) <- split ns,    lx <- results ls, 
                          ry <- results rs, res <- combine' lx ry]

-- combine each pair of results using the four numeric operators that are valid
combine' :: Result -> Result -> [Result]
combine' (l,x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

-- returns all possible expressions that solve an instance of the countdown problem
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

--------------------------Exploiting algebraic properties--------------------------------
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

-- instances of Add 2 3 are valid and Add 3 2 can be discarted since it is identical
-- Sub stays the same
-- Mul makes no sense if one of the arguments is 1
-- Div 2 1 is the same as 2, so all divisions by 1 are not considered



