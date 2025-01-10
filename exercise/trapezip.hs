matchh :: (Eq a) => [a] -> [a] -> Bool
matchh xs ys = any (uncurry (==)) (zip xs ys)

adiacenti :: (Eq a) => [a] -> Bool
adiacenti xs = matchh xs (tail xs)

ordinata :: (Ord a) => [a] -> Bool
ordinata xs = all (uncurry (<)) (zip xs (tail xs))
