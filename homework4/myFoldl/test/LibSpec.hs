module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib (ourAdd, myFoldl)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    prop "ourAdd is commutative" $ \x y ->
      ourAdd x y `shouldBe` ourAdd y x
    describe "myFoldl" $ do
      it "myFoldl works for a list of integers" $ do
        myFoldl (+) 0 [1,2,3::Integer] `shouldBe` foldl (+) 0 [1,2,3::Integer]
