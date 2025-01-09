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

media :: [Int] -> Float
media xs = (fromIntegral (foldr (+) 0 xs) / fromIntegral (length xs))

fact2 :: Int -> Int
fact2 x = foldl (*) 1 [1 .. x]

intervallo :: Int -> Int -> [Int]
intervallo m n
  | m > n = []
  | m == n = [n]
  | otherwise = m : intervallo (m + 1) n

primi :: Int -> [Int]
primi x = aux x 2
  where
    aux x n
      | x < n = []
      | primo n = n : aux x (n + 1)
      | otherwise = aux x (n + 1)

primo :: Int -> Bool
primo n = aux 2
  where
    aux k
      | k >= n = k == n
      | n `mod` k == 0 = False
      | otherwise = aux (k + 1)
