takeInput :: IO String
takeInput =
  getLine >>= \s ->
    if null s
      then return []
      else takeInput >>= \st -> return (s ++ " " ++ st)

main :: IO ()
main = takeInput >>= putStrLn
