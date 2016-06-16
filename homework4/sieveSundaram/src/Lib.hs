-- | A library to do stuff.
module Lib
    (
      ourAdd
    , sieveSundaram
    ) where

-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 1+(2*x)) $ filterListFromList removed initList
  where
    initList  = [1..n]
    removed   = [ i+j+(2*i*j)
             | i <- initList
             , j <- initList
             , i <= j
             , i+j+(2*i*j)<=n
             ]
    filterListFromList ys xs =
      filter (\x -> foldr (\y acc -> if y == x then False else acc ) True ys) xs
