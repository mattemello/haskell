primo :: Integer -> Bool
primo n = aux 2
  where
    aux k
      | k >= n = k == n
      | n `mod` k == 0 = False
      | otherwise = aux (k + 1)

primoMaggioreDi :: Integer -> Integer
primoMaggioreDi x = head (filter primo (enumFrom (succ x)))

primiGemelli :: Int -> [(Integer, Integer)]
primiGemelli x = take x (filter (gemelli (zip ps (tail ps))))
  where
    ps = filter primo (enumFrom 2)
    gemelli (n, m) = m == n + 2
