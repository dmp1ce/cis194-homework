-- | A library to do stuff.
module Party
    (
      glCons
    , moreFun
    , treeFold
    , nextLevel
    ) where

import Data.Tree
import Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL list fun) = GL (e:list) (fun + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl0 gl1
  | gl0 >= gl1  = gl0
  | otherwise   = gl1

-- Exercise 2
-- Looked up solution here: https://github.com/evansb/cis194-hw/blob/master/spring_2013/hw8/Party.hs
-- And here: https://github.com/surganov/cis194/blob/master/08/Party.hs
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node r xs) = f r (map (treeFold f) xs)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)]
  -> (GuestList, GuestList)
nextLevel b (xs) =
  ((GL [b] (empFun b)) `mappend` (snd totalGuestLists),
    (fst totalGuestLists))
  where
    totalGuestLists = foldr appendTuples (mempty,mempty) xs
    appendTuples = (\x acc ->
      ((fst x) `mappend` (fst acc),(snd x) `mappend` (snd acc)) )
