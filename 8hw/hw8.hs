module HW08 where

import Data.Maybe (isJust)
import Data.Monoid

import Text.Read (readMaybe)
import Data.List (stripPrefix, sort)
import Control.Monad.Random

allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

-- exercise 1
stripAs :: String -> Int -> Maybe String
stripAs s x = stripPrefix (replicate x 'a') s

stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go where
    go :: String -> Maybe String
    go "" = Just ""
    go s = readMaybe (take 1 s) >>= stripAs (drop 1 s) >>= go

testStringFitsFormat :: Bool
testStringFitsFormat = goodCasesTrue && badCasesFalse where
    goodCases = ["", "3aaa2aa", "9aaaaaaaaa", "0", "001a", "2aa2aa"]
    badCases = ["3aaa2a", "10aaaaaaaaaa", "1", "100a", "2bb2bb"]
    goodCasesTrue = allTrue $ map stringFitsFormat goodCases
    badCasesFalse = not $ allTrue $ map stringFitsFormat badCases

-- exercise 2
specialNumbers :: [Int]
specialNumbers = [x | x <- [1..100], x `mod` 5 == 0, x `mod` 7 /= 0]

testSpecialNumbers :: Bool
testSpecialNumbers = specialNumbers == [5, 10, 15, 20, 25, 30, 40, 45, 50, 55, 60,
                                         65, 75, 80, 85, 90, 95, 100]

-- Risk stuff
type StdRand = Rand StdGen
type Army = Int
data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army }
                deriving Show
type DieRoll = Int

-- exercise 3
dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1, 6)

-- exercise 4
instance Monoid ArmyCounts where
    mempty = ArmyCounts {attackers = 0, defenders = 0}
    mappend ArmyCounts {attackers = a1, defenders = d1}
            ArmyCounts {attackers = a2, defenders = d2} =
                ArmyCounts {attackers = a1 + a2, defenders = d1 + d2}
instance Eq ArmyCounts where
    (==) ArmyCounts {attackers = a1, defenders = d1}
         ArmyCounts {attackers = a2, defenders = d2} = a1 == a2 && d1 == d2

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults attacks defenses = foldr pairFight mempty $ zip (descending attacks) (descending defenses) where
    descending = reverse . sort
    pairFight (a, d) counts
              | a > d = ArmyCounts {attackers = 0, defenders = -1} <> counts
              | otherwise = ArmyCounts {attackers = -1, defenders = 0} <> counts

testBattleResults :: Bool
testBattleResults = battleResults [3, 6, 4] [5, 5] == ArmyCounts {attackers = -1, defenders = -1} &&
                    battleResults [3, 6, 4] [5, 6] == ArmyCounts {attackers = -2, defenders = 0} &&
                    battleResults [4] [3, 2] == ArmyCounts {attackers = 0, defenders = -1}

-- exercise 5
battle :: ArmyCounts -> StdRand ArmyCounts
battle ac@(ArmyCounts {attackers = a, defenders = d}) = do
  attackRolls <- sequence (replicate (min (a - 1) 3) dieRoll)
  defRolls <- sequence (replicate (min d 2) dieRoll)
  return $ battleResults attackRolls defRolls <> ac

-- exercise 6
invade :: ArmyCounts -> StdRand ArmyCounts
invade ac@(ArmyCounts {attackers = a, defenders = d})
       | d == 0 || a < 2 = return ac
       | otherwise = battle ac >>= invade

-- exercise 7
(//) :: Int -> Int -> Double
a // b = fromIntegral a / fromIntegral b

successProbN :: Int -> ArmyCounts -> StdRand Double
successProbN trials ac = sequence (replicate trials (invade ac)) >>= tally where
    success (ArmyCounts {defenders = d}) = d == 0
    tally invades = return $ length (filter success invades) // trials

successProb :: ArmyCounts -> StdRand Double
successProb = successProbN 1000
