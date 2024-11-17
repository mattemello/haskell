data ForseInt = Niente | Proprio Int
  deriving (Show)

testa :: [Int] -> ForseInt
testa [] = Niente
testa (x : xs) = Proprio x

-- ↓
data Numero = In Int | Fl Float deriving (Show)

somma :: Numero -> Numero -> Numero
somma (In x) (In y) = In (x + y)
somma (In x) (Fl y) = Fl (fromIntegral x + y)
somma (Fl x) (In y) = Fl (x + fromIntegral y)
somma (Fl x) (Fl y) = Fl (x + y)

sommatoria :: [Numero] -> Numero
sommatoria = foldr somma (In 0)

-- ↑

proprio :: [ForseInt] -> [Int]
proprio [] = []
proprio (Niente : xs) = proprio xs
proprio (Proprio x : xs) = x : proprio xs
