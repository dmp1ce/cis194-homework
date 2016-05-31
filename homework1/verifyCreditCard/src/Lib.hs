-- | A library to do stuff.
module Lib
    (
      ourAdd
    , doubleEveryOther
    , toDigits
    , sumDigits
    , validate
    ) where

-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y

-- Double every second digit beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs)
  | even $ length xs  = x*2:y:doubleEveryOther xs
  | otherwise         = x:y*2:doubleEveryOther xs
doubleEveryOther x     = x

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0    = []
  | otherwise = toDigits (x `div` 10) ++ [(x `mod` 10)]

sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits (x:xs)
  | x < 10        = x + (sumDigits xs)
  | otherwise     = (sumDigits $ toDigits x) + (sumDigits xs)

validate :: Integer -> Bool
validate a =
  ((sumDigits $ doubleEveryOther $ toDigits a)
  `mod` 10) == 0
