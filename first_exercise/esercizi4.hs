{- primo   esercizio -}
sume :: Int -> Int
sume 0 = 0
sume n = n + sume(n-1)

{- secondo esercizio -}

pow2 :: Int -> Int
pow2 0 = 1
pow2 n = 2 * pow2(n - 1)

{- terzo   esercizio -}

bits :: Int -> Int
bits n | n == 0         = 0
       | n `mod` 2 == 0 = bits (n `div` 2)
       | otherwise      = 1 + bits (n `div` 2)

{- quarto  esercizio -}

potenzaDi2 :: Int -> Bool
potenzaDi2 0 = False
potenzaDi2 1 = True
potenzaDi2 n =  n `mod` 2 == 0 && potenzaDi2(n `div` 2)
