module Lesson11 where

import Control.Applicative

pair :: Applicative f => f a -> f b -> f (a,b)
pair = liftA2 (,)

maybePair = pair (Just "hello") (Just 100)
listPair = pair ['a','b','c'] [1,2,3]
zipListPair = pair (ZipList ['a','b','c']) (ZipList [1,2,3])
ioPair = pair (putStr "Hello") (putStr "Goodbye")

(*>) :: Applicative f => f a -> f b -> f b
(*>) fa fb = (\_ b -> b) <$> fa <*> fb

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA aTofb [] = pure []
mapA aTofb (x:xs) = (:) <$> (aTofb x) <*> (mapA aTofb xs)

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA []      = pure []
sequenceA (x:xs)  = (:) <$> x <*> Lesson11.sequenceA xs

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA i fa
  | i > 0     = (:) <$> fa <*> replicateA (i-1) fa
  | otherwise = pure []
