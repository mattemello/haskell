prodotto :: [Int] -> Int
prodotto [] = 0
prodotto (x : xs)
  | null xs = x
  | otherwise = x * prodotto xs

inverti :: [Int] -> [Int]
inverti [] = []
inverti (x : xs) = inverti xs ++ [x]

sommacongiunta :: [Int] -> [Int] -> [Int]
sommacongiunta [] _ = []
sommacongiunta _ [] = []
sommacongiunta (x : xs) (y : ys) = (x + y) : sommacongiunta xs ys
