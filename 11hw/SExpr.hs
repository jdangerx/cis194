{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (:) <$> p <*> zeroOrMore p <|> pure []

zeroOrMoreWorks :: Bool
zeroOrMoreWorks = runOn "" == Just ("", "") &&
                  runOn "a" == Just ("a", "") &&
                  runOn "aaa" == Just ("aaa", "") &&
                  runOn "aaab" == Just ("aaa", "b")
  where runOn = runParser $ zeroOrMore (char 'a')

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

oneOrMoreWorks :: Bool
oneOrMoreWorks = runOn "" == Nothing &&
                 runOn "ab" == Just ("a", "b") &&
                 runOn "aaab" == Just ("aaa", "b")
  where runOn = runParser $ oneOrMore (char 'a')

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

spacesWorks :: Bool
spacesWorks = runOn "" == Just ("", "") &&
              runOn "aaa" == Just ("", "aaa") &&
              runOn "  aaa" == Just ("  ", "aaa")
  where runOn = runParser spaces

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> (oneOrMore (satisfy isAlphaNum))

identWorks :: Bool
identWorks = runOn "" == Nothing &&
             runOn "aaa" == Just ("aaa", "") &&
             runOn "123" == Nothing &&
             runOn "aaa123 second" == Just ("aaa123", " second")
  where runOn = runParser ident

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


parseAtom :: Parser Atom
parseAtom = spaces *> (N <$> posInt) <|> (I <$> ident) <* spaces

parseList :: Parser [SExpr]
parseList = spaces *> char '(' *> (oneOrMore parseSExpr) <* spaces <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = (A <$> parseAtom) <|> (Comb <$> parseList)
