module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Gen (choose)
import Test.QuickCheck.Property (forAll)

import Lib (ourAdd, sieveSundaram)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    prop "ourAdd is commutative" $ \x y ->
      ourAdd x y `shouldBe` ourAdd y x
    describe "sieveSundaram" $ do
      it "works to 10" $ do
        sieveSundaram 10 `shouldBe` [3,5,7,11,13,17,19] 
      prop "not empty up to 1000" $ forAll (choose (1,1000)) $ \x ->
        sieveSundaram x `shouldSatisfy` (not . null)
