module ParserSpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

--import Lib (ourAdd)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Parser" $ do
    it "works" $ do
      True `shouldBe` True
--    prop "ourAdd is commutative" $ \x y ->
--      ourAdd x y `shouldBe` ourAdd y x
