module HW1 where

-- Exercises 1-4: Validating Credit Cards
toDigits :: (Integral a) => a -> [a]
toDigits n
    | n < 10 = [n]
    | otherwise = toDigits (n `div` 10) ++ n `mod` 10:[]

doubleEveryOther :: (Num a) => [a] -> [a]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
    | even (length xs) =  x:doubleEveryOther xs
    | otherwise = 2 * x:head xs:doubleEveryOther (tail xs)

sumDigits :: (Integral a) => [a] -> a
sumDigits = sum . map sum . map toDigits

validate :: (Integral a) => a -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

-- Exercise 5: Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source dest temp
    | n == 1 = [(source, dest)]
    | otherwise = hanoi (n - 1) source temp dest ++
                  [(source, dest)] ++
                  hanoi (n-1) temp dest source
