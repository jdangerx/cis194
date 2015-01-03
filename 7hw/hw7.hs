module HW07 where

import System.Random as R
import Data.List (transpose)

-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2
fibs2 :: [Integer]
fibs2 = [0,1] ++ zipWith (+) fibs2 (tail fibs2)

-- exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- exercise 4
instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- exercise 5
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

testStreamRepeat :: Bool
testStreamRepeat = observed == expected where
    observed = take 10 . streamToList $ streamRepeat 5
    expected = take 10 $ repeat 5

streamMap :: (a -> a) -> Stream a -> Stream a
streamMap fn (Cons x stream) = Cons (fn x) $ streamMap fn stream

testStreamMap :: Bool
testStreamMap = observed == expected where
    observed = take 10 . streamToList . streamMap (+1) $ streamRepeat 5
    expected = take 10 $ repeat 6

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn seed = Cons seed (streamFromSeed fn (fn seed))

testStreamFromSeed :: Bool
testStreamFromSeed = observed == expected where
    observed = take 5 $ streamToList $ streamFromSeed ('x' :) "o"
    expected = ["o", "xo", "xxo", "xxxo", "xxxxo"]

testStreamUtils :: Bool
testStreamUtils = testStreamRepeat && testStreamMap && testStreamFromSeed

-- exercise 6
nats :: Stream Integer
nats = streamFromSeed (+1) 0

testNats :: Bool
testNats = [0..10] == take 10 (streamToList nats)

ruler :: Stream Integer
ruler = streamMap (getPow 0) (streamFromSeed (+1) 1) where
    getPow acc n
           | n `mod` 2 == 0 = getPow (acc+1) (n `div` 2)
           | otherwise = acc

testRuler :: Bool
testRuler = [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4] == take 16 (streamToList ruler)

-- exercise 7
randomList :: (R.Random a, R.RandomGen g) => g -> [a]
randomList gen = map fst . streamToList . streamFromSeed getNewRandom $ (R.random gen) where
    getNewRandom (_, newGen) = R.random newGen

-- exercise 8
randomInts :: Int -> [Int]
randomInts n = take n $ randomList (R.mkStdGen 8)

-- exercise 9
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax xs = Just (minimum xs, maximum xs)
--  1,712,059,648 bytes allocated in the heap
--    104,076,808 bytes copied during GC
--     27,460,616 bytes maximum residency (9 sample(s))
--      6,217,008 bytes maximum slop
--             63 MB total memory in use (0 MB lost due to fragmentation)

--                                   Tot time (elapsed)  Avg pause  Max pause
-- Gen  0      3296 colls,     0 par    0.05s    0.05s     0.0000s    0.0001s
-- Gen  1         9 colls,     0 par    0.06s    0.06s     0.0063s    0.0231s

-- INIT    time    0.00s  (  0.00s elapsed)
-- MUT     time    0.75s  (  0.75s elapsed)
-- GC      time    0.11s  (  0.11s elapsed)
-- EXIT    time    0.00s  (  0.00s elapsed)
-- Total   time    0.87s  (  0.86s elapsed)

-- %GC     time      12.5%  (12.5% elapsed)

-- Alloc rate    2,276,157,068 bytes per MUT second

-- Productivity  87.4% of total user, 87.6% of total elapsed

-- exercise 10
minMax' :: Maybe (Int, Int) -> [Int] -> Maybe (Int, Int)
minMax' result [] = result
minMax' Nothing (x:xs) = minMax' (Just (x, x)) xs
minMax' (Just (lower, upper)) (x:xs)
        | x < lower = minMax' (Just (x, upper)) xs
        | x > upper = minMax' (Just (lower, x)) xs
        | otherwise = minMax' (Just (lower, upper)) xs
--  1,712,060,752 bytes allocated in the heap
--      1,327,232 bytes copied during GC
--         44,312 bytes maximum residency (2 sample(s))
--         21,224 bytes maximum slop
--              1 MB total memory in use (0 MB lost due to fragmentation)

--                                   Tot time (elapsed)  Avg pause  Max pause
-- Gen  0      3303 colls,     0 par    0.01s    0.01s     0.0000s    0.0003s
-- Gen  1         2 colls,     0 par    0.00s    0.00s     0.0001s    0.0002s

-- INIT    time    0.00s  (  0.00s elapsed)
-- MUT     time    0.79s  (  0.79s elapsed)
-- GC      time    0.01s  (  0.01s elapsed)
-- EXIT    time    0.00s  (  0.00s elapsed)
-- Total   time    0.81s  (  0.81s elapsed)

-- %GC     time       1.4%  (1.4% elapsed)

-- Alloc rate    2,154,610,834 bytes per MUT second

-- Productivity  98.5% of total user, 98.8% of total elapsed
