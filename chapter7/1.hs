apply :: (a -> b) -> (a -> Bool) -> [a] -> [b]
apply f p xs = [f x | x <- xs, p x]

apply1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
apply1 f p xs = map f (filter (p) xs)
