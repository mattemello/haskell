-- Maybe
maybeLength :: Maybe a -> Int
maybeLength Nothing = 0
maybeLength (Just a) = 1

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just a) = Just (f a)

maybeFilter :: (a -> Bool) -> Maybe a -> Maybe a
maybeFilter _ Nothing = Nothing
maybeFilter f (Just a)
  | f a = Just a
  | otherwise = Nothing

-- Either
somma :: Either Int Float -> Either Int Float -> Either Int Float
somma (Left m) (Left n) = Left (m + n)
somma (Left m) (Right n) = Right (fromIntegral m + n)
somma (Right m) (Left n) = Right (m + fromIntegral n)
somma (Right m) (Right n) = Right (m + n)

-- List
data List a = Nil | Cons a (List a)

lengthList :: List a -> Int
lengthList = aux 0
  where
    aux n Nil = n
    aux n (Cons _ xs) = aux (n + 1) xs

-- Stream
data Stream a = Cont a (Stream a)

forever :: a -> Stream a
forever n = Cont n (forever n)

from :: (Enum a) => a -> Stream a
from n = Cont n (from (succ n))

takeSt :: Int -> Stream a -> [a]
takeSt 0 _ = []
takeSt n (Cont x xs) = x : takeSt (n - 1) xs
