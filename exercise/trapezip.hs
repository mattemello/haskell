matchh :: (Eq a) => [a] -> [a] -> Bool
matchh xs ys = any (uncurry (==)) (zip xs ys)

adiacenti :: (Eq a) => [a] -> Bool
adiacenti xs = matchh xs (tail xs)
