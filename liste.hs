{- correct one -}
sumaraze :: [Int] -> Bool
sumaraze xs = last xs == aux 0 (take (length xs - 1) xs)
  where
    aux n [] = n
    aux n (y : ys) = aux (n + y) ys

sumaraze2 :: [Int] -> Bool
sumaraze2 xs = last xs == sum (take (length xs - 1) xs)

sottoLista :: [Int] -> [Int] -> Bool
sottoLista [] _ = True
sottoLista _ [] = False
sottoLista xs ys = aux xs ys
  where
    aux xs ys
      | head xs == head ys = sottoLista (tail xs) (tail ys)
      | otherwise = sottoLista xs (tail ys)

sottoLista2 :: [Int] -> [Int] -> Bool
sottoLista2 [] _ = True
sottoLista2 [_] [] = False
sottoLista2 xs ys = aux xs ys ys
  where
    aux [] _ _ = sottoLista2 (tail xs) ys
    aux as bs cs
      | head as == head bs = sottoLista2 (tail as) cs
      | otherwise = aux as (tail bs) cs
