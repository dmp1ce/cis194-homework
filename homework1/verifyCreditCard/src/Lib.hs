-- | A library to do stuff.
module Lib
    (
      ourAdd
    , doubleEverySecondDigitFromRight
    ) where

import Numeric.Natural

-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y

-- Double every second digit beginning from the right
doubleEverySecondDigitFromRight :: [Natural] -> [Natural]
doubleEverySecondDigitFromRight (x:y:xs)
  | even $ length xs  = x*2:y:doubleEverySecondDigitFromRight xs
  | otherwise         = x:y*2:doubleEverySecondDigitFromRight xs
doubleEverySecondDigitFromRight x     = x
