bools :: [Bool]
bools = [True, False] 

nums :: [[Int]]
nums = [[0..1]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply f a = f a

-- try: apply copy "hi"

