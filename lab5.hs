primesMy :: [Integer]
primesMy = 2 : [x | x <- [3..], isPrimes x primesMy]
  where
    isPrimes x (y:ys)
      | y * y > x = True
      | x `mod` y == 0 = False
      | otherwise = isPrimes x ys

factorizeMy :: Integer -> [Integer]
factorizeMy a = go a primesMy
  where
    go 1 _ = []
    go x (p:ps)
      | p > x = [1]
      | otherwise = c : go rest ps
      where
        (rest, c) = count x p 0

    count a b acc
      | a `mod` b == 0 = count (a `div` b) b (acc + 1)
      | otherwise = (a, acc)

defactorizeMy :: [Integer] -> Integer
defactorizeMy powers = product (zipWith (^) primesMy powers)


-- data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show
-- Leaf   :: a -> Tree a
-- Branch :: Tree a -> Tree a -> Tree a


-- data Tree a = Empty | Branches a [Tree a] deriving Show
-- Empty    :: Tree a
-- Branches :: a -> [Tree a] -> Tree a

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

fringeMy :: Tree a -> [a]
fringeMy (Leaf x) = [x]
fringeMy (Branch l r) = fringeMy l ++ fringeMy r

mapTreeMy :: (a -> b) -> Tree a -> Tree b
mapTreeMy f (Leaf x) = Leaf (f x)
mapTreeMy f (Branch l r) = Branch (mapTreeMy f l) (mapTreeMy f r)

depthMy :: Tree a -> Integer
depthMy (Leaf _) = 1
depthMy (Branch l r) = 1 + max (depthMy l) (depthMy r)

dfsMy :: Tree a -> [a]
dfsMy (Leaf x) = [x]
dfsMy (Branch l r) = dfsMy l ++ dfsMy r

data SearchTree a = Empty | Branches a (SearchTree a) (SearchTree a) deriving (Show, Eq)
-- Empty :: SearchTree a
-- Branches :: a -> SearchTree a -> SearchTree a -> SearchTree a


elemTreeMy :: Ord a => SearchTree a -> Bool
elemTreeMy Empty = True
elemTreeMy (Branches x left right) = 
    allValues (< x) left && 
    allValues (> x) right && 
    elemTreeMy left && 
    elemTreeMy right
  where
    allValues _ Empty = True
    allValues p (Branches v l r) = p v && allValues p l && allValues p r

checkTreeMy :: (Ord a, Eq a) => a -> SearchTree a -> Bool
checkTreeMy _ Empty = False
checkTreeMy x (Branches v l r)
  | x == v = True
  | x < v = checkTreeMy v l
  | otherwise = checkTreeMy v r

putValMy :: Ord a => a -> SearchTree a -> SearchTree a
putValMy y Empty = Branches y Empty Empty
putValMy y (Branches x l r)
  | y == x = Branches x l r                   -- элемент есть, ничего не делаем
  | y < x  = Branches x (putValMy y l) r
  | otherwise  = Branches x l (putValMy y r)

list2treeMy :: Ord a => [a] -> SearchTree a
list2treeMy = foldl (flip putValMy) Empty

heightMy :: SearchTree a -> Integer
heightMy Empty = 0
heightMy (Branches _ l r) = 1 + max (heightMy l) (heightMy r)