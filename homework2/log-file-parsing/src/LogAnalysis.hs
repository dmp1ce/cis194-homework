-- | A library to do stuff.
module LogAnalysis
    (
      parseMessage
    , parseErrorWords
    , isStringDigit
    , isCharDigit
    , parse
    , buildTree
    , isLogBefore
    , insert
    , build
    , getMessageTreeTimeStamp
    , getLogMessageTimeStamp
    ) where

import Log

build :: [LogMessage] -> MessageTree
build []      = Leaf
build (x:xs)  = insert x (build xs)

-- Insert
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg Leaf      = Node Leaf logMsg Leaf
insert logMsg (Node left nodeLogMsg right)
  -- Insert logMessage can be inserted between current and right
  | logMsgTimeStamp >= nodeTimeStamp &&
    (logMsgTimeStamp <= rightNodeTimeStamp ||
    right == Leaf) =
    Node left nodeLogMsg (Node Leaf logMsg right)
  -- Insert must go somewhere on the right tree
  | right /= Leaf &&
    logMsgTimeStamp > rightNodeTimeStamp  =
    Node left nodeLogMsg (insert logMsg right)
  -- Insert must go somewhere on the left tree
  | logMsgTimeStamp < nodeTimeStamp =
    Node (insert logMsg left) nodeLogMsg right
  where
    logMsgTimeStamp     = getLogMessageTimeStamp logMsg
    nodeTimeStamp       = getLogMessageTimeStamp nodeLogMsg
    rightNodeTimeStamp  = getMessageTreeTimeStamp right
insert _ _              = error "Missing match on insert"

-- Get timestamp functions
getMessageTreeTimeStamp :: MessageTree -> Maybe TimeStamp
getMessageTreeTimeStamp (Node _ logMsg _) = getLogMessageTimeStamp logMsg
getMessageTreeTimeStamp Leaf              = Nothing
getLogMessageTimeStamp :: LogMessage -> Maybe TimeStamp
getLogMessageTimeStamp (LogMessage _ timestamp _) = Just timestamp
getLogMessageTimeStamp (Unknown _)                = Nothing

-- Build the message tree
-- I did this function before I read the entire homework.
-- See the `build` function for the Excersie 3 solution.
buildTree :: [LogMessage] -> MessageTree
buildTree []     = Leaf
buildTree ((LogMessage msgType timestamp msg):xs)  =
  Node
    (buildTree (filter (isLogBefore timestamp) xs)) -- Left branc
    (LogMessage msgType timestamp msg)
    (buildTree (filter (not . isLogBefore timestamp) xs))  -- Right branch
buildTree (Unknown _:xs)  = buildTree xs

isLogBefore :: TimeStamp -> LogMessage -> Bool
isLogBefore compare_time (LogMessage _ msg_time _)
  | compare_time > msg_time = True
  | otherwise               = False
isLogBefore _ _             = False

parse :: String -> [LogMessage]
parse []  = []
parse l   = map parseMessage $ lines l

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
