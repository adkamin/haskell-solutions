data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | BiImply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop 
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop 
p5 = BiImply (Var 'A') (Not (Var 'A'))

p6 :: Prop
p6 = BiImply (Not (Not (Var 'A'))) (Var 'A')

type Subst = Assoc Char Bool

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k ==k']

eval :: Subst -> Prop -> Bool
eval _ (Const b)     = b
eval s (Var x)       = find x s
eval s (Not p)       = not (eval s p)
eval s (And p q)     = eval s p && eval s q
eval s (Imply p q)   = eval s p <= eval s q
eval s (BiImply p q) = eval s p == eval s q

-- returns a list of all the variables in a proporsition
vars :: Prop -> [Char]
vars (Const _)     = []
vars (Var x)       = [x]
vars (Not p)       = vars p
vars (And p q)     = vars p ++ vars q
vars (Imply p q)   = vars p ++ vars q
vars (BiImply p q) = vars p ++ vars q

-- produces all combinations of Bool values of a given size
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

-- removes duplicates from a list
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- produces all prossible substitutions for a proposition 
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

-- checks if a proposition evaluates to True for all possible substitutions
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]