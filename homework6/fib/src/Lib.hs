-- | A library to do stuff.
module Lib
    (
      fib
    , fibs1
    , fibs2
    , Stream (Cons)
    , streamToList
    , streamRepeat
    ) where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n > 1 =fib (n-1) + fib (n-2) 
  | otherwise = error "Error fib cannot be calculated"

fibs1 :: [Integer]
fibs1 = map (fib) [0..]

-- Used reference here: https://github.com/evansb/cis194-hw/blob/master/spring_2013/hw6/Fibonacci.hs
fibs2 :: [Integer]
fibs2 = fibgen 0 1 where
  fibgen :: Integer -> Integer -> [Integer]
  fibgen a b = a : fibgen b (a + b) -- Instead of calculating the fib again just add the last two numbers in the squence together

-- Used reference here: https://github.com/evansb/cis194-hw/blob/master/spring_2013/hw6/Fibonacci.hs
-- Exercise 3
streamToList :: Stream a -> [a]
streamToList (Cons x rest) = x:(streamToList rest)

data Stream a = Cons a (Stream a)
instance Show a => Show (Stream a) where
--  --show a = loop (20::Int) a where
  show  = loop (20::Int)  where
            loop 0  _ = "\n"
            loop n (Cons x0 rest) = show x0 ++ ", " ++ loop (n - 1) rest

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)
