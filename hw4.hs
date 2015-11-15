import Data.List

-- Exercise #1: Wholemeal programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate transformation
  where transformation n | even n    = n `div` 2
                         | otherwise = 3 * n + 1


-- Exercise #2: Folding with trees

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr buildTree Leaf

buildTree :: a -> Tree a -> Tree a
buildTree x Leaf = Node 0 Leaf x Leaf
buildTree x (Node h l a r)
  | hL > hR   = let newRight = buildTree x r in Node (newHeight hL newRight) l a newRight
  | otherwise = let newLeft  = buildTree x l in Node (newHeight hR newLeft) newLeft a r
  where hL               = height l
        hR               = height r
        newHeight h tree = succ $ max h (height tree)

height :: Tree a -> Integer
height Leaf           = -1
height (Node h _ _ _) = h


-- strategy: make a queue of nodes at each successive level

-- Exercise #3: More Folds!

xor :: [Bool] -> Bool
xor = odd . length . filter (== True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- (Optional)
-- Determined based on the Prelude signatures:
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldr :: (a -> b -> b) -> b -> [a] -> b

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)


-- Exercise #4: Finding Primes via Sieve of Sundaram

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x*2 + 1) $ [1..upperBound] \\ toRemove
  where upperBound = 2*n + 2
        toRemove   = [x | i <- [1..upperBound], j <- [1..upperBound], let x = i + j + 2 * i * j,
                          1 <= i,
                          i <= j,
                          x <= upperBound]
