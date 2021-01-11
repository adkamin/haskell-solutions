data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"
   show Exp = "^"

-- applies an operator to two positive naturals, returns another positive natural 
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

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
choices = concat . map perms . subs

-- returns all possible ways of splitting a list into two non-empty lists that append
-- to give the original list
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- combines a pair of expressions using each of the four numberic operators
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

type Result = (Expr,Int)

-- combine each pair of results using the four numeric operators that are valid
combine' :: Result -> Result -> [Result]
combine' (l,x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

-- returns all possible expressions that solve an instance of the countdown problem
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

-- returns all possible results whose list of values is precisely a given list
results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns  = [res | (ls,rs) <- split ns,    lx <- results ls, 
                          ry <- results rs, res <- combine' lx ry]

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0
valid Exp _ y = y >= 0

-- returns the list of closest solutions up to a given bound of tries
findClosest :: [Int] -> Int -> Int -> [Expr]
findClosest ns n 0     = solutions' ns n
findClosest ns n bound = if null sols then (findClosest ns (n-1) (bound-1)) 
                                           ++ (findClosest ns (n+1) (bound-1))
                            else sols
                               where sols = solutions' ns n

main :: IO ()
main = print (resutls [1,2])                -- implementation of exponentiation 
       print (findClosest [1,2] 5 2)        -- returning the closest results
       --print (order (findClosest [1,2] 3 2) -- ordering


