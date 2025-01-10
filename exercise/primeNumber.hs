primo :: Integer -> Bool
primo n = aux 2
  where
    aux k
      | k >= n = k == n
      | n `mod` k == 0 = False
      | otherwise = aux (k + 1)

primoMaggioreDi :: Integer -> Integer
primoMaggioreDi n = head (filter primo (enumFrom (n+1)))

primiGemelli :: Int -> [(Integer, Integer)]
primiGemelli n = take n (filter gemelli (zip lk (tail lk)))
    where
        lk = (filter primo ([2..]))
        gemelli (p, q) = q == p + 2 
