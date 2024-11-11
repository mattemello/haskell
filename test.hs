data Stream a = Cons a (Stream a)

from :: (Enum a) => a -> Stream a
from x = Cons x (from (succ x))
