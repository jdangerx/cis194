-- Exercises 1-4
toDigits :: (Integral a) => a -> [a]
toDigits n
    | n < 10 = [n]
    | otherwise = toDigits (n `div` 10) ++ n `mod` 10:[]

doubleEverySecond :: (Num a) => [a] -> [a]
doubleEverySecond [] = []
doubleEverySecond (x:xs)
    | even (length xs) =  x:doubleEverySecond xs
    | otherwise = 2 * x:head xs:doubleEverySecond (tail xs)

sumDigits :: (Integral a) => [a] -> a
sumDigits = foldl1 (\ acc x -> sum (toDigits x) + acc)

validate :: (Integral a) => a -> Bool
validate n = sumDigits (doubleEverySecond (toDigits n)) `mod` 10 == 0

-- Exercise 5: Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source dest temp
    | n == 1 = [(source, dest)]
    | otherwise = hanoi (n - 1) source temp dest ++ [(source, dest)] ++ hanoi (n-1) temp dest source
