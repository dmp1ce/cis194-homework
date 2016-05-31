-- | A library to do stuff.
module LogAnalysis
    (
      ourAdd
    , parseMessage
    ) where

import Log

-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y

parseMessage :: String -> LogMessage
parseMessage s = Unknown s
