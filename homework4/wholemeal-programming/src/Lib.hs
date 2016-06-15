-- | A library to do stuff.
module Lib
    (
      ourAdd
    , fun1
    , fun1'
    , fun2
    , fun2'
    ) where

-- Allow a modifier to QuickCheck for very small integers
--import Test.QuickCheck.Arbitrary
--import Test.QuickCheck.Gen
--instance Arbitrary Integer where
--  arbitrary = choose (0, 1000)

-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * (fun1 xs)
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (\acc x -> if even x then (x-2)*acc else acc) 1

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1)
  . iterate (\x -> if even x then x `div` 2 else  x * 3 + 1)
-- sum . takeWhile (notNull) iterate (\x ->
--  if even x
--    then x `div` 2
--    else 3 * x + 1)
