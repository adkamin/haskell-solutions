-- converts a function on pairs into a curried function
curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x,y)

-- converts a curried function into a function on pair
uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y
