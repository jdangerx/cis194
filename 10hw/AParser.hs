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

instance Applicative Parser where
  pure a = Parser (\ s -> Just (a, s))
  (Parser f1) <*> (Parser f2) = Parser f
    where
      f [] = Nothing
      f s = case f1 s of
              Nothing -> Nothing
              Just (fab, s') -> first fab <$> f2 s'

-- exercise 3:

-- part 1: expects to see the chars 'a' and 'b' and returns ('a', 'b')
-- basically we can lift (,) into the context of Parser Char
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParserWorks:: Bool
abParserWorks = runParser abParser "abcd" == Just (('a', 'b'), "cd") &&
                runParser abParser "acd" == Nothing &&
                runParser abParser "bcd" == Nothing &&
                runParser abParser "cdab" == Nothing


-- part 2: returns () instead of ('a', 'b')
-- we can replace (,) in abParser with function that takes 2 args and returns ()
abParser_ :: Parser ()
abParser_ = mkEmpty <$> char 'a' <*> char 'b'
  where mkEmpty _ _ = ()

abParser_Works:: Bool
abParser_Works = runParser abParser_ "abcd" == Just ((), "cd") &&
                 runParser abParser_ "acd" == Nothing &&
                 runParser abParser_ "bcd" == Nothing &&
                 runParser abParser_ "cdab" == Nothing

-- exercise 4: implement an Alternative instance for Parser since
-- Maybe already has an Alternative instance, we can lift (<|>) into
-- the (String ->) context and apply (<|>) to the Maybe (a, String) bits.
instance Alternative Parser where
  empty = Parser {runParser = pure Nothing}
  (Parser f1) <|> (Parser f2) = Parser $ (<|>) <$> f1 <*> f2


-- exercise 5:
intOrUppercase :: Parser ()
intOrUppercase = (mkEmpty <$> satisfy isUpper) <|> (mkEmpty <$> posInt)
  where mkEmpty _ = ()

intOrUppercaseWorks :: Bool
intOrUppercaseWorks = runParser intOrUppercase "1abcd" == Just ((), "abcd") &&
                      runParser intOrUppercase "Aabcd" == Just ((), "abcd") &&
                      runParser intOrUppercase "abcd" == Nothing &&
                      runParser intOrUppercase "a1bcd" == Nothing &&
                      runParser intOrUppercase "aAbcd" == Nothing
                      
