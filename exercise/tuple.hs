scambia :: (Int, Int) -> (Int, Int)
scambia (x, y) = (y, x)

ordina :: (Int, Int, Int) -> (Int, Int, Int)
ordina (x, y, z)
  | x <= y && y <= z = (x, y, z)
  | x > y = ordina (y, x, z)
  | otherwise = ordina (x, z, y)
