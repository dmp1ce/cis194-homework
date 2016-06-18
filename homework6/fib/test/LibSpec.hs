module LibSpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

import Lib (fib)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "Simple fib - exercise 1" $ do
      fib 4 `shouldBe` 3
--    it "works" $ do
--      True `shouldBe` True
--    prop "ourAdd is commutative" $ \x y ->
--      ourAdd x y `shouldBe` ourAdd y x
