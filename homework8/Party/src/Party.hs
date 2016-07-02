-- | A library to do stuff.
module Party
    (
      glCons
    ) where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL list fun) = GL (e:list) (fun + empFun e)
