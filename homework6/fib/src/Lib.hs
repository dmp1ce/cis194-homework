{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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
    , x
    , fibs3
    , Matrix (Matrix)
    , fib4
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
streamToList (Cons y rest) = y:(streamToList rest)

data Stream a = Cons a (Stream a)
instance Show a => Show (Stream a) where
--  --show a = loop (20::Int) a where
  show  = loop (20::Int)  where
            loop 0  _ = "\n"
            loop n (Cons x0 rest) = show x0 ++ ", " ++ loop (n - 1) rest

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat y = Cons y (streamRepeat y)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons y rest) = Cons (f y) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons (seed) (streamFromSeed f (f seed))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x' xs) (Cons y ys) =
  Cons x' (Cons y (interleaveStreams xs ys))

-- I was close but I needed a little help from this solution:
-- https://github.com/gfixler/cis194/blob/master/hw06/Fibonacci.hs
-- I needed to have "Cons x" to start the stream to prevent recurive loop in
-- looking for the beginning of the stream.
ruler :: Stream Integer
ruler = genRuler 0 where
  genRuler :: Integer -> Stream Integer
  genRuler y = Cons y $ interleaveStreams (genRuler (y+1)) (streamRepeat y)

-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger i     = Cons i (streamRepeat 0)
  negate (Cons i r) = Cons (negate i) (negate r)
  (+) (Cons i0 r0) (Cons i1 r1) = Cons (i0+i1) (r0 + r1)
  -- a0b0 + x(a0B' + A'B)
  (*) (Cons i0 r0) (Cons i1 r1) = Cons (i0*i1)
    (streamMap (*i0) r1 + r0 * (Cons i1 r1))


instance Fractional (Stream Integer) where
  -- Q = A/B
  -- Q = a0/b0 + x((1/b0) + ((1/b0)(A' - QB'))
  (/) (Cons i0 r0) (Cons i1 r1) =
    Cons (i0 `div` i1)              -- a0/b0 +
      $ streamMap (*(1 `div` i1))   -- x((1/b0)
        (r0                         -- + (A'
        - (Cons i0 r0) * r1 / (Cons i1 r1)) -- - QB'))

fibs3 :: Stream Integer
fibs3 = x / ((1::Stream Integer) - x - x^(2::Integer)) 

-- Exercise 7
--                    C 1     C 2
data Matrix = Matrix  Integer Integer -- Row 1
                      Integer Integer -- Row 2
  deriving (Show, Eq)

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
    Matrix  (a11*b11 + a12*b21) (a11*b12 + a12*b22)
            (a21*b11 + a22*b21) (a12*b21 + a22*b22)

--instance Integral Matrix where
--    toInteger (Matrix a11 _ _ _)  = a11

fib4 :: Integer -> Integer
fib4 0  = 0
fib4 1  = 1
fib4 x' = toInteger' $ (Matrix 1 1 1 0)^(x'-1) where
  toInteger' (Matrix a11 _ _ _) = a11
