module LogSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Modifiers

import Log
import LogAnalysis  (
                      parseMessage
                    , parseErrorWords
                    , isCharDigit
                    , isStringDigit
                    )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Log Analysis" $ do
    --it "works" $ do
    --  True `shouldBe` True
    --prop "ourAdd is commutative" $ \x y ->
    --  ourAdd x y `shouldBe` ourAdd y x
    describe "Main parse functions" $ do
      it "parseMessage example 1 - Error" $ do
        parseMessage "E 2 562 help help" `shouldBe`
          LogMessage (Error 2) 562 "help help"
      prop "parseMessage error code of different digits" $
        \(Positive x) (Positive y) ->
        parseMessage ("E " ++ (show x) ++ " " ++ (show y) ++ " help help")
          `shouldBe` LogMessage (Error x) y "help help"
      it "parseErrorWords example 1" $ do
        parseErrorWords ["2", "562", "help", "help"] `shouldBe`
          LogMessage (Error 2) 562 "help help"
      it "Error code a string" $ do
        parseErrorWords ["abc", "562", "help", "help"] `shouldBe`
          Unknown "E abc 562 help help"
      it "Timestamp in error message is a string" $ do
        parseErrorWords ["1", "abc", "help", "help"] `shouldBe`
          Unknown "E 1 abc help help"
      it "parseMessage exampe 2 - Info" $ do
        parseMessage "I 29 la la la" `shouldBe`
          LogMessage Info 29 "la la la"
      it "parseMessage Info timestamp not a number" $ do
        parseMessage "I abc la la la" `shouldBe`
          Unknown "I abc la la la"
      it "Completely wrong Message" $ do
        parseMessage "fefef@#$#fefefeddd" `shouldBe`
          Unknown "fefef@#$#fefefeddd"

    describe "isDigit functions" $ do
      -- Test isDigit functinos
      it "isCharDigit works for 1" $ do
        isCharDigit '1'`shouldBe` True
      it "isStringDigit works for '123' string" $ do
        isStringDigit "123" `shouldBe` True
      it "isStringDigit works for '1' string" $ do
        isStringDigit "1" `shouldBe` True
      it "isStringDigit works non-digit" $ do
        isStringDigit "abc" `shouldBe` False
