module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib (ourAdd, doubleEverySecondDigitFromRight)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    prop "ourAdd is commutative" $ \x y ->
      ourAdd x y `shouldBe` ourAdd y x
    prop "double every number from the right" $ \a b c d ->
      doubleEverySecondDigitFromRight [a,b,c,d] `shouldBe` [a*2, b, c*2, d]
