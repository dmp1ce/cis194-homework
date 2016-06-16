module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib (ourAdd, xor)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    prop "ourAdd is commutative" $ \x y ->
      ourAdd x y `shouldBe` ourAdd y x
    describe "xor" $ do
      it "example 1" $ do
        xor [False, True, False] `shouldBe` True
      it "example 2" $ do
        xor  [False, True, False, False, True] `shouldBe` False
