myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat xs = foldl (++) [] xs

invert :: [a] -> [a]
invert = foldr (\z ys -> ys ++ [z]) []

myany :: (a -> Bool) -> [a] -> Bool
myany f = foldr (||) False . map f

prova :: [Int] -> Bool
prova xs = myall (== 2) xs

myall :: (a -> Bool) -> [a] -> Bool
myall f = foldr (&&) True . map f

membro :: (Eq a) => a -> [a] -> Bool
membro x ys = foldl (\x -> \y -> x == y) x ys
