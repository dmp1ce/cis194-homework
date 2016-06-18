-- | A library to do stuff.
module Lib
    (
      fib
    , fibs1
    ) where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n > 1 =fib (n-1) + fib (n-2) 
  | otherwise = error "Error fib cannot be calculated"

fibs1 :: [Integer]
fibs1 = map (\x -> fib x) [0..]
