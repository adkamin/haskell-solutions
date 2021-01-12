-- doubles a digit and subtracts 9 if the resulting number is greater than 9
luhnDouble :: Int -> Int
luhnDouble x = 
   if x*2 > 9 then x*2 - 9 
   else x*2

-- applies two functions alternatively to elements in a list
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 []     = [] 
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs

-- determines if a four-digit bank card number is valid
luhnOld :: Int -> Int -> Int -> Int -> Bool
luhnOld a b c d = ((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10 == 0

-- determindes if a bank card number of any length is valid
luhn :: [Int] -> Bool
luhn digits = sum (altMap luhnDouble id digits) `mod` 10 == 0
