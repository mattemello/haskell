{- personalFoldl :: (a -> b -> b) -> b -> [a] -> b
personalFoldl _ x [] = x
personalFoldl f x (y : ys) = personalFoldl f (f x y) ys -}

concatt :: [[a]] -> [a]
concatt xs = foldl (++) [] xs

anyy :: (a -> Bool) -> [a] -> Bool
anyy f = foldr (||) False . map f

alll :: (a -> Bool) -> [a] -> Bool
alll f = foldr (&&) True . map f

massimo :: (Ord a) => [a] -> a
massimo (x : xs) = foldr max x xs

occorrenza :: (Eq a) => a -> [a] -> Int
occorrenza x = length . filter (== x)

membro :: (Eq a) => a -> [a] -> Bool
membro x = any (== x)
