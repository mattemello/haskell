union :: (Ord a) => [a] -> [a] -> [a]
union [] ys = ys
union xs [] = xs
union (x : xs) (y : ys)
  | x == y = x : union xs ys
  | x < y = x : union xs (y : ys)
  | otherwise = y : union (x : xs) ys

intersection :: (Ord a) => [a] -> [a] -> [a]
intersection [] _ = []
intersection (x : xs) ys = aux x ys
  where
    aux b [] = intersection xs ys
    aux b (c : cs)
      | b == c = b : intersection xs ys
      | otherwise = aux b cs

difference :: (Ord a) => [a] -> [a] -> [a]
difference [] _ = []
difference (x : xs) ys = aux x ys
  where
    aux b [] = b : difference xs ys
    aux b (c : cs)
      | b == c = difference xs ys
      | otherwise = aux b cs
