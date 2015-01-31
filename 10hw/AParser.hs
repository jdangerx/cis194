{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- exercise 1: make a Functor instance of Parser
first :: (a -> b) -> (a, c) -> (b, c)
first f pair = (f . fst $ pair, snd pair)

instance Functor Parser where
  fmap f (Parser runParserA) = Parser (fmap (first f) . runParserA)


-- exercise 2: make an Applicative instance of Parser
second :: (b -> c) -> (a, b) -> (a, c)
second f pair = (fst pair, f . snd $ pair)

-- instance Applicative Parser where
--   pure a = Parser (\ s -> Just (a, s))
--   p1@(Parser f1) <*> p2@(Parser f2) = Parser (_ <*> f2)

  -- p1 :: Parser (a -> b) == Parser (String -> Maybe (a -> b, String))
  -- p2 :: Parser a == Parser (String -> Maybe (a, String))
  -- result :: Parser (String -> Maybe (b, String))

  -- f1 :: String -> Maybe (a -> b, String)
  -- f2 :: String -> Maybe (a, String)
  -- f3 :: String -> Maybe (b, String)

  -- String -> Maybe (b, String)
