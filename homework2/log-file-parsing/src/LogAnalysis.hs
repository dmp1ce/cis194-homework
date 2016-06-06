-- | A library to do stuff.
module LogAnalysis
    (
      parseMessage
    , parseErrorWords
    , isStringDigit
    , isCharDigit
    ) where

import Log

parseMessage :: String -> LogMessage
parseMessage s
  | letterCode == "E" = parseErrorWords $ tail msg_words
  | letterCode == "I" || letterCode == "W" = parseWords $ msg_words
  | otherwise         = Unknown s
  where
    msg_words   = words s
    letterCode  = head msg_words

parseErrorWords :: [String] -> LogMessage
parseErrorWords w
  | isStringDigit (head w) == False ||
    isStringDigit (w !! 1) == False = Unknown ("E " ++ unwords w)
  | otherwise = LogMessage (Error errorCode) timestamp ((unwords . drop 2) w)
  where
    errorCode = (read . head) w
    timestamp = read $ w !! 1

parseWords :: [String] -> LogMessage
parseWords w
  | isStringDigit (w !! 1) == False = Unknown (unwords w)
  | letterCode == 'I' = LogMessage Info timestamp ((unwords . drop 2) w)
  | letterCode == 'W' = LogMessage Warning timestamp ((unwords . drop 2) w)
  | otherwise = Unknown (unwords w)
  where
    letterCode = (head . head) w
    timestamp = read (w !! 1)

-- Functions to determine if read can get integers from string
isStringDigit :: [Char] -> Bool
isStringDigit []              = False
isStringDigit (x:[])          = isCharDigit x
isStringDigit (x:xs)
  | t == False  = False
  | t == True   = isStringDigit xs
    where
      t = isCharDigit x
isStringDigit _               = False

isCharDigit :: Char -> Bool
isCharDigit '1' = True
isCharDigit '2' = True
isCharDigit '3' = True
isCharDigit '4' = True
isCharDigit '5' = True
isCharDigit '6' = True
isCharDigit '7' = True
isCharDigit '8' = True
isCharDigit '9' = True
isCharDigit '0' = True
isCharDigit _   = False

-- Test matching strings
--testParseString :: String -> String
--testParseString ('E':xs) = xs
--testParseString _ = "Nothing matched"
