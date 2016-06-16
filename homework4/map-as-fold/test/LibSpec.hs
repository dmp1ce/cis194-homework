module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib (ourAdd, map')

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    prop "ourAdd is commutative" $ \x y ->
      ourAdd x y `shouldBe` ourAdd y x
    describe "map'" $ do
      let myList = [0,1,2,3,4,5::Integer]
      it "map' works with list of integers" $ do
        map' (+1) myList  `shouldBe` map (+1) myList
      let myStrings = ["hello", "1", "more"]
      it "map' works with list of string to bool" $ do
        map' (=="1") myStrings `shouldBe` map (=="1") myStrings
