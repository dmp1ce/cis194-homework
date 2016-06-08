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
    , inOrder
    , whatWentWrong
    , whatWentWrongAllErrors
    , isInMessage
    , whatWentWrongFilterString
    ) where

import Log
import Data.List (isInfixOf)

-- I tried to filter the messages with this string filter but all I could see
-- was a bunch of Alice in Wonderland passages.
customFilter :: [String]
customFilter = ["000","BIOS","ACPI","0x","pci","64bit","OEM","TL 23","MEM",
  "scsizing","nullizer","1ff","wlats","#5987","ACPU","wlan","MSI","#27",
  "PME#","WDC00","sdhcd","irq","APIC","cfff","DMA","RAM","#10","ffff"]

whatWentWrongFilterString :: [LogMessage] -> [String]
whatWentWrongFilterString []      = []
whatWentWrongFilterString x  = map getLogMessageString (
    filter isWarning (
      filter ((not . isInMessage customFilter)) ((inOrder . build) x)
    )
  )

whatWentWrongAllErrors :: [LogMessage] -> [String]
whatWentWrongAllErrors []      = []
whatWentWrongAllErrors x  = map getLogMessageString (filter (isSeverityOf 0) ((inOrder . build) x))

--whatWentWrongAllInfo :: [LogMessage] -> [String]
--whatWentWrongAllInfo []      = []
--whatWentWrongAllInfo x  = map getLogMessageString ((inOrder . build) x)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong []      = []
whatWentWrong x  = map getLogMessageString (filter (isSeverityOf 50) ((inOrder . build) x))

getLogMessageString :: LogMessage -> String
getLogMessageString (LogMessage _ _ s)  = s
getLogMessageString (Unknown s)         = s

isWarning :: LogMessage -> Bool
isWarning (LogMessage Warning _ _) = True
isWarning _ = False

-- Return true of the LogMessage of greater than specified severity
isSeverityOf :: Int -> LogMessage -> Bool
isSeverityOf threshold (LogMessage (Error severity) _ _)
  | severity >= threshold = True
  | otherwise             = False
isSeverityOf _ _  = False

-- Return True if the one of the list of words is in message
isInMessage :: [String] -> LogMessage -> Bool
isInMessage [] _          = False
isInMessage (x:xs) (LogMessage logType time msg)
  | x `isInfixOf` msg     = True
  | otherwise             = isInMessage xs (LogMessage logType time msg)
isInMessage (x:xs) (Unknown msg)
  | x `isInfixOf` msg     = True
  | otherwise             = isInMessage xs (Unknown msg)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                      = []
inOrder (Node left logMsg right)  = (inOrder left) ++ [logMsg] ++ (inOrder right)

-- Exercise 3 solution
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
