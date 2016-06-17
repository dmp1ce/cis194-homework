-- | A library to do stuff.
module Lib
    (
      eval
    ) where

import ExprT

--data ExprT = Lit Integer
--           | Add ExprT ExprT
--           | Mul ExprT ExprT
--  deriving (Show, Eq)

-- Exercise 1
eval :: ExprT -> Integer
eval (Mul e1 e2)  = (eval e1) * (eval e2)
eval (Add e1 e2)  = (eval e1) + (eval e2)
eval (Lit i)      = i
