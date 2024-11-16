union :: (Ord a) => [a] -> [a] -> [a]
union [] [] = []
union [] (y : ys) = y : union [] ys
union (x : xs) [] = x : union xs []
union (x : xs) (y : ys)
  | x == y = x : union xs ys -- WARNING: la list non deve contenere duplicati, hai sbagliato questo
  | x < y = x : union xs (y : ys)
  | otherwise = y : union (x : xs) ys

intersection :: (Ord a) => [a] -> [a] -> [a]
intersection _ [] = []
intersection xs ys = aux xs ys xs
  where
    aux [] (_ : ys) zs = intersection zs ys
    aux (x : xs) (y : ys) zs
      | x == y = y : aux xs (y : ys) zs
      | otherwise = aux xs (y : ys) zs

difference :: (Ord a) => [a] -> [a] -> [a]
difference [] _ = []
difference (x : xs) ys
  | aux x ys = x : difference xs ys
  | otherwise = difference xs ys
  where
    aux _ [] = True
    aux x (y : ys)
      | x == y = False
      | otherwise = aux x ys
