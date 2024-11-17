import Data.Sequence (unzipWith)

myMatch :: (Eq a) => [a] -> [a] -> Bool
myMatch xs ys = any (uncurry (==)) (zip xs ys)

adiacenti :: (Eq a) => [a] -> Bool
adiacenti xs = any (uncurry (==)) (zip xs (tail xs))

polinomio :: [Float] -> Float -> Float
polinomio xs y = sum (map (uncurry (*)) (zip xs (map (y ^) [0 ..])))

perfetto :: Int -> Bool
perfetto x = x == sum (filter ((== 0) . (x `mod`)) [1 .. x - 1])

ordinata :: (Ord a) => [a] -> Bool
ordinata xs = all (uncurry (<=)) (zip xs (tail xs))
