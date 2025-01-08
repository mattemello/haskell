-- esercizi laboratio 2 "dall'iterazione all ricorsione"

-- 1°
fact :: Int -> Int
fact 0 = 1
fact x = x * fact (x - 1)  

-- 2°
bits :: Int -> Int
bits = aux 0
    where 
        aux res 0 = res 
        aux res n = aux (res + n `mod` 2) (n `div` 2)

-- 3°
eucli :: Int -> Int -> Int
eucli m n | m == n = n
          | otherwise = aux m n
    where 
        aux m n | m < n = eucli (m) (n - m)
                | otherwise = eucli (m - n) (n)

