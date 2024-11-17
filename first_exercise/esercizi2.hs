{- first  exsercise -}

massimo :: Int -> Int -> Int
massimo x y | x < y      = y 
            | otherwise  = x 

{- second exsercise -}

minimo :: Int -> Int -> Int
minimo x y = not . massimo(x y)
