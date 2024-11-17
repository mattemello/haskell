{- Insert Sort -}
insertSort :: [Int] -> [Int]
insertSort []       = []
insertSort (x : xs) = insert x (insertSort xs)
    where 
        insert x [] = [x]
        insert x (y : ys) | x <= y      = x : y : ys
                          | otherwise   = y : insert x ys

{- Marge Sort -}

{- split :: [Int] -> ([Int], [Int])
split []            = ([], [])
split [x]           = ([x], [])
split (x : y : xs)  = (x : ys, y : zs)
    where
        (ys, zs) = split xs -}

split :: [Int] -> ([Int], [Int])
split xs = (take n xs, drop n xs)        
    where
        n = length xs `div` 2

merge :: [Int] -> [Int] -> [Int]
merge []    ys  = ys
merge xs    []  = xs
merge (x : xs) (y : ys) | x <= y    = x : merge xs        (y : ys)
                        | otherwise = y : merge (x : xs)  ys

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort ys) (mergeSort zs)
    where 
        (ys, zs) = split xs

{- Quick Sort -}

filter :: Ord a => (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) | p x = x : filter p xs
                  | otherwise = filter p xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
    quickSort (filter (< x) xs) ++ [x] ++ quickSort (filter (>= x) xs)
