-- | A library to do stuff.
module Lib
    (
      eval
    , evalStr
    ) where

import ExprT
import Data.Maybe (isNothing)
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
