isOdd :: Int -> Bool
isOdd = \x -> x `mod` 2 == 0

isEven :: Int -> Bool
isEven = not . isOdd
