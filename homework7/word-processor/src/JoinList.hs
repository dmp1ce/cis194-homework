{-#LANGUAGE FlexibleInstances #-}
-- | A library to do stuff.
module JoinList
    (
      JoinList (Single, Append, Empty)
    , (+++)
    , (!!?)
    , jlToList
    , indexJ
    , dropJ
    , ValidJoinList (ValidJoinList)
--    , tag
    ) where
-- For testing JoinList arbitrary
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Sized
--import Data.Maybe (isNothing)

-- Exercise 1
data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _)  = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl  = jl
(+++) jl Empty  = jl
(+++) jl1 jl2   = Append (mappend (tag jl1) (tag jl2)) jl1 jl2

-- Exercise 2
-- For testing with QuckCheck on JoinList data types
instance (Arbitrary a, Arbitrary b) => Arbitrary (JoinList a b) where
  arbitrary = do
    x <- arbitrary
    s <- arbitrary
    jl0 <- arbitrary
    jl1 <- arbitrary
    result <- elements [(Single s x), (Append s jl0 jl1), Empty]
    return $ result

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Create QuickCheck modifier for valid JoinList
newtype ValidJoinList m a = ValidJoinList (JoinList m a)
  deriving ( Eq, Show )
instance (Arbitrary a) => Arbitrary (ValidJoinList Size a) where
  arbitrary = ValidJoinList <$> do
    x <- arbitrary
    ValidJoinList jl0 <- arbitrary
    ValidJoinList jl1 <- arbitrary
    result <- elements
                  [
                    (Single (Size 1) x)
                  , (Append (addJoinListSizes jl0 jl1) jl0 jl1)
                  , Empty
                  ]
    return $ result

addJoinListSizes :: JoinList Size a -> JoinList Size a -> Size
addJoinListSizes jl0 jl1 = Size (jlSize jl0 + jlSize jl1)
  where
    jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
    jlSize = getSize . size . tag

isValidJoinList :: (Sized s, Monoid s) => JoinList s a -> Bool
isValidJoinList jl
    | jl_size > 0 = True
    | otherwise   = False
    where
      jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
      jlSize = getSize . size . tag
      jl_size = jlSize jl

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ index (Append _ jl0 jl1)
  | jl0_size > 0 && jl0_size >= (index+1)
              = indexJ index jl0
  | jl1_size > 0 && jl1_size >= (index+1) - jl0_size
              = indexJ (index - jl0_size) jl1
  | otherwise = Nothing
  where
    jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
    jlSize = getSize . size . tag
    jl0_size = jlSize jl0
    jl1_size = jlSize jl1
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n (Append m jll jlr)
  | n >= jl_size            = Empty
  | n == jll_size           = jlr
  | n > jll_size            = dropJ (n-jll_size) jlr
  | n < jll_size            = dropJ n jll +++ jlr
  | otherwise               = (Append m jll jlr)
  where
    jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
    jlSize = getSize . size . tag
    jll_size  = jlSize jll
    jl_size   = (getSize $ size m)
dropJ n jl
  | n >= (getSize $ size $ tag jl)  = Empty
  | otherwise                       = jl
