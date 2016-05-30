module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Modifiers

import Lib  (
    ourAdd
  , doubleEveryOther
  , toDigits
  , sumDigits
  , validate
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    prop "ourAdd is commutative" $ \x y ->
      ourAdd x y `shouldBe` ourAdd y x
    prop "double every number from the right" $ \a b c d ->
      doubleEveryOther [a,b,c,d] `shouldBe` [a*2, b, c*2, d]
    it "double every other Example 1" $ do
      doubleEveryOther  [8,7,6,5] `shouldBe` [16,7,12,5]
    it "double every other Example 2" $ do
      doubleEveryOther  [1,2,3] `shouldBe` [1,4,3]
    it "convert digit into list Example 1" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]
    it "convert digit into list Example 2" $ do
      toDigits 4321 `shouldBe` [4,3,2,1]
    prop "negative toDigits should be []" $ \(NonNegative a) ->
      toDigits (-a) `shouldBe` []
    it "One digit works for toDigits" $ do
      toDigits 1 `shouldBe` [1]
    it "0 toDigits should be []" $ do
      toDigits 0 `shouldBe` []
    it "Sum digits Example 1" $ do
      sumDigits [16,7,12,5] `shouldBe` 22
    it "validate Example 1" $ do
      validate 4012888888881881 == True
    it "validate Example 2" $ do
      validate 4012888888881882 == False
