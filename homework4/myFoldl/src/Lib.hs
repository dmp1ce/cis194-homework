-- | A library to do stuff.
module Lib
    (
      ourAdd
    , myFoldl
    ) where

-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> f acc x) base $ reverse xs

-- See https://wiki.haskell.org/Foldl_as_foldr for this solution
--myFoldl f base xs = foldr (\b g x -> g (f x b)) id xs base
