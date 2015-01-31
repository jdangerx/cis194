module Week4 where
import Data.List
-- 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x = (x - 2) * fun1 xs
     | otherwise = fun1 xs

-- we only multiply by (x-2) when x is even, we do nothing when x is odd
-- so we can ignore all the odd elements, subtract 2 from everything, and
-- multiply everything together
fun1' :: [Integer] -> Integer
fun1' = foldr1 (*) . map (+(-2)) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- We keep getting the next number in the Collatz sequence but only add it
-- to our result if it's even - so we just generate the Collatz sequence,
-- filter for even-ness, and then sum it
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (\x -> x /= 1) .
        iterate (\n -> case even n of True -> n `div` 2
                                      False -> 3 * n + 1)

-- 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight Leaf = (-1)
getHeight (Node height _ _ _) = height

treeInsert :: a -> Tree a -> Tree a
treeInsert elt Leaf = Node 0 Leaf elt Leaf
treeInsert elt (Node h l c r)
       | getHeight l < getHeight r = Node h (treeInsert elt l) c r
       | getHeight l == getHeight r =
           Node (getHeight (treeInsert elt l) + 1) (treeInsert elt l) c r
       | otherwise = Node h l c (treeInsert elt r)

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

-- 3.1
xor :: [Bool] -> Bool
xor = foldr (\x y -> if x then not y else y) False

-- 3.2
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x y -> f x : y) [] xs

-- 4
sieveTsundere :: Integer -> [Integer]
sieveTsundere n = map ((+1) . (*2)) $ filter (`notElem` badNums) [1..n] where
    badNums = [x + y + 2*x*y | x <- [1..n], y <- [1..n], x+y+2*x*y <= n]
