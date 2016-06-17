-- | A library to do stuff.
module Lib
    (
      eval
    , evalStr
    , Expr (mul,add,lit)
    , testExp
    , MinMax (MinMax)
    , Mod7 (Mod7)
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
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr Integer where
  mul x y = x * y
  add x y = x + y
  lit x   = x

instance Expr Bool where
  mul x y = x && y
  add x y = x || y
  lit x   = x > 0

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  add (MinMax x) (MinMax y) = MinMax (max x y)
  lit x                     = MinMax x

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)
  lit x   = Mod7 (x `mod` 7)
