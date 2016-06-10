module GolfSpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

import Golf (skips, localMaxima, histogram)

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
    describe "localMaxima functionality" $ do
      it "Example 1" $ do
        localMaxima  [2,9,5,6,1] `shouldBe` [9,6]
      it "Example 2" $ do
        localMaxima  [2,3,4,1,5] `shouldBe` [4]
      it "Example 3" $ do
        localMaxima  [1,2,3,4,5] `shouldBe` []
      it "Example 4" $ do
        localMaxima [] `shouldBe` []
    describe "histogram functionality" $ do
      it "Example 1" $ do
        histogram [1,1,1,5] `shouldBe`
          " *        \n *        \n *   *    \n=========\n0123456789\n"
      it "Example 2" $ do
        histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe`
          "    *     \n    *     \n    * *   \n ******  *\n=========\n0123456789\n"
      it "Example 3" $ do
        histogram [3,5] `shouldBe`
          "   * *    \n=========\n0123456789\n"
      it "Emtpy histogram" $ do
        histogram [] `shouldBe`
          "=========\n0123456789\n"
