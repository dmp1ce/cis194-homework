module LibSpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

import ExprT
import Lib (eval)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
      it "eval example 1" $ do 
        eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
  --  it "works" $ do
  --    True `shouldBe` True
  --  prop "ourAdd is commutative" $ \x y ->
  --    ourAdd x y `shouldBe` ourAdd y x
