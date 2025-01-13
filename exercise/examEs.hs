notEqual :: (Eq a) => [a] -> [a] -> Bool
notEqual [] _ = False
notEqual (x : xs) ys = aux x ys
  where
    aux x [] = True
    aux x (z : zs)
      | z == x = notEqual xs ys
      | otherwise = aux x zs

inversioni :: (Ord a) => [a] -> Int
inversioni [_] = 0
inversioni (x : xx : xs)
  | x > xx = 1 + inversioni (xx : xs)
  | otherwise = inversioni (xx : xs)
