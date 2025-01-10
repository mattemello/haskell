lastSum :: [Int] -> Bool
lastSum = aux 0 
    where
        aux ris [x] = ris == x
        aux ris (x : xs) = aux (ris + x) xs

lstSum :: [Int] -> Bool
lstSum xs = head rxs == foldr (+) 0 (tail rxs)
    where
        rxs = reverse xs

maxList :: [[a]] -> [[a]]
maxList xs = filter (((maxLenght xs) ==) . length) xs
    where
        maxLenght xs = maximum (map (length) xs)

mapp :: (a -> b) -> [a] -> [b]
mapp f xs = foldr ((:) . (f)) [] xs  
