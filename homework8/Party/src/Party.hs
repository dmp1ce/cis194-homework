-- | A library to do stuff.
module Party
    (
      glCons
    , moreFun
    , treeFold
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
