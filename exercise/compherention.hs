mapp :: (a -> b) -> [a] -> [b]
mapp f ys = [f x | x <- ys]

filterr :: (a -> Bool) -> [a] -> [a]
filterr f ys = [x | x <- ys, f x]

primo :: (Integral a) => a -> Bool
primo x = 0 == length [y | y <- [2 .. x - 1], (x `mod` y == 0)]
