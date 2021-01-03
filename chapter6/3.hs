import Prelude hiding ((^))

(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m ^ (n-1))

{-
  2 ^ 3
=  { applying ^ }
  2 * (2 ^ 2)
=  { applying ^ }
  2 * (2 * (2 ^ 1))
=  { applying ^ }
  2 * (2 * (2 * (2 ^ 0)))
=  { applying ^ }
  2 * (2 * (2 * (1)))
=  { applying * }
  8
-}
