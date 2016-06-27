module ScrabbleSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Scrabble

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Scrabble spec" $ do
    prop "Basic monoid associative test" $ (\x y ->
      Score x + Score y `shouldBe` Score y + Score x)
    prop "Basic monoid empty test" $ (\x ->
      Score x + mempty `shouldBe` Score x)
    prop "Letters have scores" $ forAll (elements (['a'..'z'] ++ ['A'..'Z'])) $
      (\c -> (getScore $ score (c)) > 0)
