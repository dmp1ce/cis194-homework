{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- Looked up solution here: https://github.com/evansb/cis194-hw/blob/master/spring_2013/hw11/SExpr.hs
-- oneOrMore p will give Nothing if parser finds nothing, then pure [] will be fore zeroOrMore
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- oneOrMore uses the first parse and then zeroOrMore p.
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- Looked up solution because I forgot that I needed to use "satisfy" and not just isSpace.
-- https://github.com/evansb/cis194-hw/blob/master/spring_2013/hw11/SExpr.hs#L24
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

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

-- Had part of parseAtom but looked up solution for the Comb constructor.
-- It was helpful to know that the entire patern could be described using various
-- parsers and recusively called using oneOrMore.
-- https://github.com/evansb/cis194-hw/blob/master/spring_2013/hw11/SExpr.hs#L53
parseSExpr :: Parser SExpr
parseSExpr = A <$> (spaces *> parseAtom <* spaces)
  <|> Comb <$> (spaces *> char '(' *> oneOrMore parseSExpr <* char ')' <* spaces)
  
parseAtom :: Parser Atom
parseAtom = I <$> (ident) <|> N <$> (posInt)
