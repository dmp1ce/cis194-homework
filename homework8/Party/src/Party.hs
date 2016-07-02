-- | A library to do stuff.
module Party
    (
      glCons
    ) where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons _ gl = gl
