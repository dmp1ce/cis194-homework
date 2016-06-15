module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Gen (choose)
import Test.QuickCheck.Property (forAll)

import Lib (ourAdd,fun1,fun1',fun2,fun2')

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    prop "ourAdd is commutative" $ \x y ->
      ourAdd x y `shouldBe` ourAdd y x

    describe "Exercise 1" $ do
      prop "fun1 is the same as fun1'" $ \xs ->
        fun1 xs `shouldBe` fun1' xs
      prop "fun2 is the same as fun2'" $ forAll (choose (0,1000)) $ \x ->
        fun2 x `shouldBe` fun2' x
      it "works" $ do
        fun2 1230 `shouldBe` fun2' 1230
