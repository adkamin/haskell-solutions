data Op = Add | Sub | Mul | Div

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"

-- applies an operator to two positive naturals, returns another positive natural 
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
   show (Val n)     = show n
   show (App o l r) = brak l ++ show o ++ brak r
                      where
                         brak (Val n) = show n
                         brak e       = "(" ++ show e ++ ")"

-- returns the overall value of an expression
eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

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
choices xs = [p | s <- subs xs, p <- perms s]

-- returns all possible ways of splitting a list into two non-empty lists that append
-- to give the original list
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

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

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)


