notEqualnoric :: (Eq a) => [a] -> [a] -> Bool
notEqualnoric xs ys = any (`elem` ys) xs

notEqual :: (Eq a) => [a] -> [a] -> Bool
notEqual [] _ = False
notEqual (x : xs) ys = aux x ys
  where
    aux x [] = True
    aux x (z : zs)
      | z == x = notEqual xs ys
      | otherwise = aux x zs

inversioniNoric :: (Ord a) => [a] -> Int
inversioniNoric xs = length (filter (aux) (zip xs (tail xs)))
  where
    aux (x, y) = x > y

inversioni :: (Ord a) => [a] -> Int
inversioni [_] = 0
inversioni (x : xx : xs)
  | x > xx = 1 + inversioni (xx : xs)
  | otherwise = inversioni (xx : xs)

pariLib :: [a] -> [a]
pariLib = map snd . filter (even . fst) . zip [0 ..]

pariRic :: [a] -> [a]
pariRic (x : _ : xs) = x : pariRic xs
pariRic xs = xs

lastPari :: (Integral a) => [a] -> Maybe a
lastPari = aux . filter ((== 0) . (`mod` 2))
  where
    aux [] = Nothing
    aux xs = Just (last xs)

lastPariNotric :: (Integral a) => [a] -> Maybe a
lastPariNotric xs = controll (aux xs)
  where
    aux [] = []
    aux (x : xs)
      | x `mod` 2 == 0 = x : aux xs
      | otherwise = aux xs

    controll [] = Nothing
    controll [x] = Just x
    controll (_ : xs) = controll xs

data Tree a = Empty | Node a [Tree a]

elements :: Tree a -> [a]
elements Empty = []
elements (Node x xs) = x : concatMap elements xs
