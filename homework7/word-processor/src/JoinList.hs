-- | A library to do stuff.
module JoinList
    (
      JoinList (Single, Append, Empty)
    , (+++)
    , (!!?)
    , jlToList
    , indexJ
    ) where
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
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ index (Append _ jl0 jl1)
  | index - (joinListSize jl0) >= 0 = indexJ (index - (joinListSize jl0)) jl0
  | index - (joinListSize jl1) >= 0 = indexJ (index - (joinListSize jl1)) jl1
  | otherwise                       = Nothing
indexJ _ _ = Nothing

joinListSize :: (Sized b, Monoid b) => JoinList b a -> Int
joinListSize (Single m _)   = getSize $ size m
joinListSize (Append m _ _) = getSize $ size m
joinListSize _              = 0
