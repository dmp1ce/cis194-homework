module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Log
import LogAnalysis  (
                      ourAdd
                    , parseMessage
                    )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Log Analysis" $ do
    it "works" $ do
      True `shouldBe` True
    prop "ourAdd is commutative" $ \x y ->
      ourAdd x y `shouldBe` ourAdd y x
    it "parseMessage example 1" $ do
      parseMessage "E 2 562 help help" `shouldBe`
        LogMessage (Error 2) 562 "help help"
