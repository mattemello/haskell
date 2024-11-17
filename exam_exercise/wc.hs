takeAllLines :: IO [String]
takeAllLines =
  getLine >>= \l ->
    if null l
      then return []
      else takeAllLines >>= \ls -> return (l : ls)

{- main :: IO ()
   main = do s <- getContents
          putStrLn $
            " " ++ (show $ length $ lines s) ++
            " " ++ (show $ length $ words s) ++
            " " ++ (show $ length s)
-}

wordsCount :: IO [String] -> Int
wordsCount = do
  x <- takeAllLines
  printing aux x
  where
    aux [] = 0
    aux (x : xs) = do aux2 x xs

    aux2 [] xs = 0 + aux xs
    aux2 (y : y2 : ys) xs
      | y2 == ' ' && y /= ' ' = 1 + aux2 ys xs
      | y2 == '\n' && y /= ' ' = 1 + aux2 ys xs
      | otherwise = aux2 (y2 : ys) xs

wc :: IO ()
wc = wordsCount >>= putStrLn
