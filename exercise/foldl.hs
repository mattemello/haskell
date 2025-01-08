personalFoldl :: (a -> b -> b) -> b -> [a] -> b
personalFoldl _ x [] = x
personalFoldl f x (y : ys) = personalFoldl f (f x y) ys
