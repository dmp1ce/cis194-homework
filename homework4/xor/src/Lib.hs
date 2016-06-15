-- | A library to do stuff.
module Lib
    (
      ourAdd
    , xor
    ) where

-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y

xor :: [Bool] -> Bool
xor = even . foldr (\x acc -> if x == True then (acc + 1::Integer) else acc) 0
