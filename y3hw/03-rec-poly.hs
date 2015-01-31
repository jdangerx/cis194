module Golf where

import Data.List
-- 1. Pretty boneheaded. Use zip to add indices to the lists and do some mod stuff.
everyNth :: Int -> [a] -> [a]
everyNth n xs = map (\tup -> snd tup) (filter (\tup -> fst tup `mod` n == 0) (zip [1,2..] xs))

skips :: [a] -> [[a]]
skips xs = map (\tup -> everyNth (fst tup) xs) (zip [1,2..] xs)

-- 2. Local maxima. Scan across in chunks of three.
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rest)
            | x < y && y > z = y:(localMaxima (z:rest)) -- can skip y because it can't be a thing
            | otherwise = localMaxima (y:z:rest)
localMaxima _ = []

-- 3. Histogram.

pad :: Int -> String -> String
pad n s
    | (length s < n) = pad n (" " ++ s)
    | otherwise = s

sameIntToString :: [Integer] -> String
sameIntToString [] = ""
sameIntToString (x:xs) = replicate (length xs) '*' ++ "=" ++ show x

histogram :: [Integer] -> String
histogram xs = unlines . transpose $ columns where
    groups = map sameIntToString (group (sort (xs++[0..9])))
    maxLen = maximum (map length groups)
    columns = map (pad maxLen) groups
