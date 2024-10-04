{-fattoria :: Int -> Int
fattoria n | n == 0 = 1
           | otherwise = n * fattoria(n - 1) -}

{- questa è la versione migliore -}
fattoria :: Int -> Int
fattoria 0 = 1
fattoria n = n * fattoria(n - 1)
