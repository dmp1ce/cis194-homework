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
    , takeJ
    , scoreLine
    , jlToString
    , jlFromString
    , jlSize
    , jlReplaceLine
--    , tag
    ) where
-- For testing JoinList arbitrary
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.List.Split (splitOn)

import Sized
import Scrabble

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
                  , jl0 +++ jl1
                  , Empty
                  ]
    return $ result


addJoinListSizes :: JoinList Size a -> JoinList Size a -> Size
addJoinListSizes jl0 jl1 = Size (jlSize jl0 + jlSize jl1)

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size . tag
--isValidJoinList :: (Sized s, Monoid s) => JoinList s a -> Bool
--isValidJoinList jl
--    | jl_size > 0 = True
--    | otherwise   = False
--    where
--      jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
--      jlSize = getSize . size . tag
--      jl_size = jlSize jl

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ index (Append _ jl0 jl1)
  | jl0_size > 0 && jl0_size >= (index+1)
              = indexJ index jl0
  | jl1_size > 0 && jl1_size >= (index+1) - jl0_size
              = indexJ (index - jl0_size) jl1
  | otherwise = Nothing
  where
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
    jll_size  = jlSize jll
    jl_size   = (getSize $ size m)
dropJ n jl
  | n >= (getSize $ size $ tag jl)  = Empty
  | otherwise                       = jl

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ n (Single m a)
  | n >= 1    = (Single m a)
  | otherwise = Empty
takeJ n (Append m jll jlr)
  | n >= jl_size  = (Append m jll jlr)
  | n < jll_size  = takeJ n jll
  | n == jll_size = jll
  | n > jll_size  = jll +++ takeJ (n - jll_size) jlr
  | otherwise     = error "Unexpected case"
  where
    jll_size  = jlSize jll
    jl_size   = (getSize $ size m)
takeJ _ _ = Empty

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4
jlToString :: JoinList (Score, Size) String -> String
-- It would be nice to get rid of reverse here
--jlToString jl = reverse $ drop 1 $ reverse $ jlToString' jl where
--jlToString (Single _ )  = ""
jlToString (Single _ a) = a
jlToString Empty        = []
jlToString (Append _ jl0 jl1) = jlToString jl0 ++ "\n" ++ jlToString jl1

jlFromString :: String -> JoinList (Score, Size) String
jlFromString "" = Single (Score 0, Size 1) ""
jlFromString s = foldr (\x acc -> Single (scoreString x, (Size 1)) x +++ acc) Empty (splitOn "\n" s)
--  where
--    -- Add a line break if necessary to add the one that "lines" will remove
--    fixEnding :: String -> String
--    -- It would be nice to get rid of reverse here.
--    fixEnding s' = case (reverse s') of
--      (x:_) -> if x `elem` "\n" then s' ++ "\n" else s'
--      _ -> s'

jlReplaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
jlReplaceLine _ _ b = b

-- QuickCheck arbitrary instance
instance Arbitrary (ValidJoinList (Score, Size) String) where
  arbitrary = ValidJoinList <$> do
    x <- arbitrary
    ValidJoinList jl0 <- arbitrary
    ValidJoinList jl1 <- arbitrary
    result <- elements
                  [
                    (Single (scoreString x, Size 1) x)
                  , jl0 +++ jl1
                  , Empty
                  ]
    return $ result
