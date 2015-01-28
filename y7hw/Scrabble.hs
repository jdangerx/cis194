{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char (toUpper)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

getScore :: Score -> Int
getScore (Score i) = i

-- probably a shorter way to do this, but this seems pretty readable.
-- OTOH this relies on me typing the information from the website in properly.
score :: Char -> Score
score c
  | elem upperChar "AEILNORSTU" = Score 1
  | elem upperChar "DG" = Score 2
  | elem upperChar "BCMP" = Score 3
  | elem upperChar "FHVWY" = Score 4
  | elem upperChar "K" = Score 5
  | elem upperChar "JX" = Score 8
  | elem upperChar "QZ" = Score 10
  | otherwise = Score 0
  where upperChar = toUpper c

scoreString :: String -> Score
scoreString = foldr (+) (Score 0) . map score

scoreStringWorks :: Bool
scoreStringWorks = scoreString "abcdefghijklmnopqrstuvwxyz" == Score 87 &&
                   scoreString "ABCDEFGHIJKLMnopqrstuvwxyz" == Score 87 &&
                   scoreString "Aa! ($(#" == Score 2
