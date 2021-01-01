import Prelude hiding (product)

-- produces the product of a list of numbers
product :: Num a => [a] -> a
product []     = 1
product (n:ns) = n * product ns

-- proving that product [2,3,4] = 24
--   product [2,3,4]
-- =   { applying product }
--   2 * product [3,4]
-- =   { applting product }
--   2 * (3 * product [4])
-- =   { applying product }
--   2 * (3 * (4 * product []))
-- =   { applying product }
--   2 * (3 * (4 * 1))
-- =   { applying * }
--   24
