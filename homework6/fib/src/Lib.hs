-- | A library to do stuff.
module Lib
    (
      fib
    , fibs1
    , fibs2
    , Stream (Cons)
    , streamToList
    , streamRepeat
    , streamMap
    , streamFromSeed
    , nats
    , interleaveStreams
    , ruler 
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

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x rest) = Cons (f x) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons (seed) (streamFromSeed f (f seed))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) =
  Cons x (Cons y (interleaveStreams xs ys))

-- I was close but I needed a little help from this solution:
-- https://github.com/gfixler/cis194/blob/master/hw06/Fibonacci.hs
-- I needed to have "Cons x" to start the stream to prevent recurive loop in
-- looking for the beginning of the stream.
ruler :: Stream Integer
ruler = genRuler 0 where
  genRuler :: Integer -> Stream Integer
  genRuler x = Cons x $ interleaveStreams (genRuler (x+1)) (streamRepeat x)
