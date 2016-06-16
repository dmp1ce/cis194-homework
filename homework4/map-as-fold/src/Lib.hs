-- | A library to do stuff.
module Lib
    (
      ourAdd
    , map'
    ) where

-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x):acc) [] 
