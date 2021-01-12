import Prelude hiding (mult)

mult :: Int -> (Int -> Int)
mult = \a -> (\b -> a*b)
