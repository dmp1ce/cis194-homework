module LibSpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

import ExprT
import Lib  ( eval
            , evalStr
            , Expr(lit,mul,add)
            )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
      it "eval example 1" $ do 
        eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
      it "evalStr example 1" $ do 
        evalStr "(2+3)*4" `shouldBe`
          Just (eval $ Mul (Add (Lit 2) (Lit 3)) (Lit 4))
      it "evalStr example 2" $ do 
        evalStr "2+3*4" `shouldBe`
          Just (eval $ Add (Lit 2) (Mul (Lit 3) (Lit 4)))
      it "evalStr example 3" $ do 
        evalStr "2+3*" `shouldBe`
          Nothing
      it "Expr class equals ExprT expression" $ do
        mul (add (lit 2) (lit 3)) (lit 4) `shouldBe`
          Mul (Add (Lit 2) (Lit 3)) (Lit 4)
  --  it "works" $ do
  --    True `shouldBe` True
  --  prop "ourAdd is commutative" $ \x y ->
  --    ourAdd x y `shouldBe` ourAdd y x
