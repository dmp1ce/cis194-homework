-- | A library to do stuff.
module JoinList
    (
      JoinList (Single, Append, Empty)
    , (+++)
    ) where


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
