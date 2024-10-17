myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl _ x [] = x
myfoldl f x (y : ys) = myfoldl f (f x y) ys
