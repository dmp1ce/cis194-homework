{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Data.Char
import           Control.Applicative

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
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
---- Exercise 1
first :: (a -> b) -> (a,c) -> (b,c)
-- I'm not sure if this is corect, but it compiles!
first f (x,z) = (f x,z)

-- Looked up solution here: https://github.com/evansb/cis194-hw/blob/master/spring_2013/hw10/AParser.hs
-- and here: https://github.com/BerndSchwarzenbacher/cis194-solutions/blob/master/10-applicative-functors-part1/AParser.hs
instance Functor Parser where
  -- Apply fmap function to the first element in tuple
  -- Using the 'first' function defined above as a partal composed with
  -- Parser arguments (String -> Maybe (a, String))
  -- At least that is my understanding of this functor instance
  fmap f (Parser a)    = Parser (fmap (first f) . a)

-- Exercise 2
instance Applicative Parser where
  -- Looked up solution here: https://github.com/evansb/cis194-hw/blob/master/spring_2013/hw10/AParser.hs
  -- A lamda function of type String -> Maybe (a, String) to match Parser parameter
  pure a    = Parser (\str -> Just (a, str))
  -- and here: https://github.com/BerndSchwarzenbacher/cis194-solutions/blob/master/10-applicative-functors-part1/AParser.hs
  p1 <*> p2 = Parser f
    where
      f str = case runParser p1 str of
                Nothing               -> Nothing
                Just (m, str')        -> first m <$> runParser p2 str'

-- Exercise 3
-- Looked up hint here: https://github.com/evansb/cis194-hw/blob/master/spring_2013/hw10/AParser.hs
-- We can use char function to produce a Parser by passing in a Char. Using Applicative and fmap (<$>) to form a tuple Parser
abParser :: Parser (Char, Char)
abParser = (\a b -> (a,b)) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\i1 _ i2 -> [i1,i2]) <$> posInt <*> char ' ' <*> posInt

-- Exercise 4
-- Again looked up solution here: https://github.com/evansb/cis194-hw/blob/master/spring_2013/hw10/AParser.hs
-- and then modified a little so I could better understand what was going on
instance Alternative Parser where
  empty = Parser (const Nothing)
  a <|> b = Parser pFunc where
      pFunc str = case runParser a str of
        Nothing -> runParser b str
        something -> something

-- Exercise 5
-- Again looked up solution here: https://github.com/evansb/cis194-hw/blob/master/spring_2013/hw10/AParser.hs#L91
-- After I had most but got stuck with the multiple fmaps and then an Alternative between fmap results.
-- I changed the solution a little to better understand, essentually implementing const with lamba function.
intOrUpperCase :: Parser ()
intOrUpperCase = (\_ -> ()) <$> posInt <|> (\_ -> ()) <$> satisfy (isUpper)
