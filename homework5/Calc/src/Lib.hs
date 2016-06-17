-- | A library to do stuff.
module Lib
    (
      eval
    , evalStr
    , Expr (mul,add,lit)
    ) where

import ExprT
import Parser (parseExp)

--data ExprT = Lit Integer
--           | Add ExprT ExprT
--           | Mul ExprT ExprT
--  deriving (Show, Eq)

-- Exercise 1
eval :: ExprT -> Integer
eval (Mul e1 e2)  = (eval e1) * (eval e2)
eval (Add e1 e2)  = (eval e1) + (eval e2)
eval (Lit i)      = i

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = evalMaybe $ parseExp Lit Add Mul s
  where 
    evalMaybe :: Maybe ExprT -> Maybe Integer
    evalMaybe (Just e)  = Just (eval e)
    evalMaybe Nothing   = Nothing

-- Exercise 3
class Expr a where
  mul,add :: a -> a -> a
  lit     :: Integer -> a

-- Looked up some help here:
-- https://github.com/BerndSchwarzenbacher/cis194-solutions/blob/master/05-typeclasses/Calc.hs
instance Expr ExprT where
  mul = Mul
  add = Add
  lit = Lit

-- Exercise 4
