module GolfSpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

import Golf (skips)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Golf spec" $ do
    describe "skip functionality" $ do
      it "Example 1" $ do
        skips "ABCD" `shouldBe` ["ABCD","BD","C","D"]
      it "Example 2" $ do
        skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      it "Example 3" $ do
        skips [1::Integer] `shouldBe` [[1]]
      it "Example 4" $ do
        skips [True,False] `shouldBe` [[True,False],[False]]
      it "Example 5" $ do skips ([] :: [Int]) `shouldBe` ([] :: [[Int]])
