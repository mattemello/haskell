data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show)

tmin :: Tree a -> a
tmin (Branch x Leaf _) = x
tmin (Branch _ xs _) = tmin xs

-- note: think like this
treeSort :: (Ord a) => [a] -> [a]
treeSort = elements . foldr insert Leaf

elements :: Tree a -> [a]
elements Leaf = []
elements (Branch x t₁ t₂) = elements t₁ ++ [x] ++ elements t₂

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Branch x Leaf Leaf
insert x t@(Branch y t₁ t₂)
  | x == y = t
  | x < y = Branch y (insert x t₁) t₂
  | otherwise = Branch y t₁ (insert x t₂)

empty :: Tree a -> Bool
empty Leaf = True
empty _ = False

tmax :: Tree a -> a
tmax (Branch x _ Leaf) = x
tmax (Branch _ _ t) = tmax t

bst :: (Ord a) => Tree a -> Bool
bst Leaf = True
bst (Branch x tl tr)
  | x < tmin tl && x > tmax tr = bst tl && bst tr
  | otherwise = False
