-- doubles a digit and subtracts 9 if the resulting number is greater than 9
luhnDouble :: Int -> Int
luhnDouble x = 
   if x*2 > 9 then x*2 - 9 
   else x*2

-- determines if a four-digit bank card number is valid
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = ((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10 == 0
