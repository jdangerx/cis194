{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Main where

import Data.Monoid
import Sized
import Buffer
import Scrabble
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- exercise 1
-- Turns out (+++) is just Append
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append jl1 jl2

-- Get the tag from a JoinList. Since tag is a monoid we this isn't hard.
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append jl1 jl2) = tag jl1 <> tag jl2


-- exercise 2
-- 2.1 This indexes into a JoinList, like safe indexing on a normal list
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single t content)
  | i <= singleSize = Just content
  | otherwise = Nothing
  where singleSize = getSize . size $ t
indexJ i jl@(Append jl1 jl2)
  | i > jlSize = Nothing
  | i < jl1Size = indexJ i jl1
  | otherwise = indexJ (i - jl1Size) jl2
  where
    jlSize = getSize . size $ tag jl
    jl1Size = getSize . size $ tag jl1

-- Useful for writing concise tests :)
listToJL :: [a] -> JoinList Size a
listToJL [] = Empty
listToJL [x] = Single 1 x
listToJL (x:xs) = Append (listToJL [x]) (listToJL xs)

indexJWorks :: Bool
indexJWorks = indexJ 0 (listToJL "this is fine") == Just 't' &&
              indexJ 10 (listToJL "11charslong") == Just 'g' &&
              indexJ (-1) (listToJL "no indexing by negative number") == Nothing &&
              indexJ 1000 (listToJL "index too high") == Nothing

-- 2.2
-- Drop the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ _ Empty = Empty
dropJ n jl@(Single t _)
  | n >= jlSize = Empty
  | otherwise = jl
  where jlSize = getSize . size $ t
dropJ n (Append jl1 jl2)
  | n >= jl1Size = dropJ (n - jl1Size) jl2
  | otherwise = Append (dropJ n jl1) jl2
  where jl1Size = getSize . size $ tag jl1

dropJWorks :: Bool
dropJWorks = dropJ 0 (listToJL "drop0") == listToJL "drop0" &&
             dropJ 4 (listToJL "drop4") == listToJL "4" &&
             dropJ 5 (listToJL "drop5") == listToJL "" &&
             dropJ (-1) (listToJL "drop-1") == listToJL "drop-1" &&
             dropJ 1000 (listToJL "drop1000") == Empty


-- 2.3 act like normal take, but on JoinLists
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ n jl@(Single t _)
  | n < jlSize = Empty
  | otherwise = jl
  where jlSize = getSize . size $ t
takeJ n (Append jl1 jl2)
  | n <= jl1Size = takeJ n jl1
  | otherwise = Append jl1 (takeJ (n - jl1Size) jl2)
  where jl1Size = getSize . size $ tag jl1


takeJWorks :: Bool
takeJWorks = takeJ 0 (listToJL "take0") == Empty &&
             takeJ 4 (listToJL "take4") == listToJL "take" &&
             takeJ 5 (listToJL "take5") == listToJL "take5" &&
             takeJ (-1) (listToJL "take-1") == Empty &&
             takeJ 1000 (listToJL "take1000") == listToJL "take1000"


-- exercise 3
-- Most of the machinery is in Scrabble.hs - most importantly scoreString
scoreLine :: String -> JoinList Score String
scoreline "" = Empty
scoreLine s = Single (scoreString s) s

scoreLineWorks :: Bool
scoreLineWorks = scoredLines == Append (Single (Score 9) "yay ")
                                       (Single (Score 14) "haskell!") &&
                 tag scoredLines == Score 23
                 where scoredLines = scoreLine "yay " +++ scoreLine "haskell!"


-- exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append jl1 jl2) = toString jl1 ++ "\n" ++ toString jl2
  fromString "" = Empty
  fromString s = foldr (+++) Empty $ map scoreSizeLine $ lines s
    where
      scoreSizeLine :: String -> JoinList (Score, Size) String
      scoreSizeLine "" = Empty
      scoreSizeLine l = Single (scoreString l, Size 1) l
  line = indexJ
  replaceLine i s jl = takeJ i jl +++ fromString s +++ dropJ (i + 1) jl
  numLines = getSize . snd . tag
  value = getScore . fst . tag

main :: IO ()
main = runEditor editor $ (fromString "? for help" :: JoinList (Score, Size) String)
