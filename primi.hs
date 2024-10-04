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

sommaCongiunta :: [Int] -> [Int] -> [Int]
sommaCongiunta (x : xs) (y : ys) | xs == []  = [x+y]
                                 | ys == []  = [x+y]
                                 | otherwise = [x + y] ++ sommaCongiunta xs ys
