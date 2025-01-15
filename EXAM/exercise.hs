minorLast :: (Ord a) => [a] -> Int
minorLast [x] = 0
minorLast (x : xs)
  | aux xs > x = 1 + minorLast xs
  | otherwise = minorLast xs
  where
    aux [x] = x
    aux (_ : xs) = aux xs

minorLastRic :: (Ord a) => [a] -> Int
minorLastRic xs = length (filter (last xs >) xs)
