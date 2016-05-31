module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib (ourAdd, hanoi)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    prop "ourAdd is commutative" $ \x y ->
      ourAdd x y `shouldBe` ourAdd y x
    prop "hanoi example 1" $ do
      hanoi 2 "a" "b" "c" `shouldBe`
        [("a","c"), ("a","b"), ("c","b")]
