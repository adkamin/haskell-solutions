-- the original recursive definition of factorial
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- modified with a guard to prohibit negative argument, which would cause infinite loop
safeFac :: Int -> Int
safeFac 0 = 1
safeFac n = if n > 0 then n * fac (n-1) else n
