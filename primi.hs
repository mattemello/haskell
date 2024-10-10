primo :: Int -> Bool
primo n = aux 2
    where
        aux k | k >= n         = k == n
              | n `mod` k == 0 = False 
              | otherwise      = aux (k + 1)

primi :: Int -> [Int]
primi n = aux2 2
    where
        aux2 k | k > n          = [] 
               | primo k         = [ k ] ++ aux2 (k + 1)
               | otherwise       = aux2 (k + 1)

{- esercizi di pattern matching-}

listaInversa :: [Int]->[Int]
listaInversa [] = []
listaInversa (x : xs) = listaInversa(xs)++[x] 

listaInversa2 :: [Int]->[Int]
listaInversa2 = aux []
    where
        aux ys []       = ys
        aux ys (x:xs)   = aux (x:ys) xs

sommaCongiunta :: [Int] -> [Int] -> [Int]
sommaCongiunta []       _        = []
sommaCongiunta _        []       = []
sommaCongiunta (x : xs) (y : ys) = (x + y) : sommaCongiunta xs ys
