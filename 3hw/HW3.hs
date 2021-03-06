module Golf where

import Data.List
-- 1. Pretty boneheaded. Use zip to add indices to the lists and do some mod stuff.
everyNth :: Int -> [a] -> [a]
everyNth n xs = map snd (filter (\tup -> fst tup `mod` n == 0) (zip [1,2..] xs))

skips :: [a] -> [[a]]
skips xs = map (\tup -> everyNth (fst tup) xs) (zip [1,2..] xs)

-- 2. Local maxima. Scan across in chunks of three.
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rest)
            | x < y && y > z = y:localMaxima (z:rest)
            | otherwise = localMaxima (y:z:rest)
localMaxima _ = []

-- 3. Histogram.
-- pad a string out to a certain size.
pad :: Int -> String -> String
pad n s = replicate (n - length s) ' ' ++ s

-- Take a group of n of the same ints and turn it into "*"*(n-1) ++ "=" ++ int
sameIntToString :: [Integer] -> String
sameIntToString [] = ""
sameIntToString (x:xs) = replicate (length xs) '*' ++ "=" ++ show x

-- Add [0..9] to the list of integers so we get all the integers,
-- then group, turn them into strings, and pad by the longest length.
-- Then transpose since we've been working with rows instead of columns.
histogram :: [Integer] -> String
histogram xs = unlines . transpose $ columns where
    groups = map sameIntToString (group (sort (xs++[0..9])))
    maxLen = maximum (map length groups)
    columns = map (pad maxLen) groups
