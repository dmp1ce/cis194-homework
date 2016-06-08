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
                    , isLogBefore
                    , buildTree
                    , insert
                    , build
                    , getMessageTreeTimeStamp
                    , getLogMessageTimeStamp
                    , inOrder
                    , whatWentWrong
                    , isInMessage
                    )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Log Analysis" $ do
    describe "whatWentWrong functions" $ do
      let log1 = LogMessage (Error 1) 3 "Error here"
      let log2 = LogMessage (Error 50) 4 "Serious error"
      let log3 = LogMessage (Error 100) 5 "More serious"
      let log4 = LogMessage (Error 999) 1 "Crazy times"
      it "whatWentWrong works" $ do
        whatWentWrong [log1, log2, log3, log4] `shouldBe`
          ["Crazy times", "Serious error", "More serious"]
      it "isInMessage works" $ do
        isInMessage ["hello"] (LogMessage Info 100 "fefe hello@eee") `shouldBe`
          True

    describe "inOrder function" $ do
      let log1 = LogMessage (Error 1) 3 "Error here"
      let log2 = LogMessage Info 2 "Info here"
      let log3 = LogMessage Warning 4 "Warning here"
      let tree = build [log1,log2,log3]
      it "inOrder works" $ do
        inOrder tree `shouldBe` [log2, log1, log3]

    describe "Build message tree functions" $ do
      it "isLogBefore test 1" $ do
        isLogBefore 100 (LogMessage Info 99 "hello") `shouldBe` True
      it "isLogBefore test 2" $ do
        isLogBefore 1 (LogMessage Warning 99 "hello") `shouldBe` False

      -- Define some log messages for testing
      let log1 = LogMessage (Error 1) 3 "Error here"
      let log2 = LogMessage Info 2 "Info here"
      let log3 = LogMessage Warning 4 "Warning here"
      let tree1 = buildTree [log1,log2,Unknown "uhoh",log3]
      it "buildTree simple example" $ do
        tree1 `shouldBe` Node (Node Leaf log2 Leaf) log1 (Node Leaf log3 Leaf)

      -- Define LogMessage to insert
      let log4 = LogMessage Info 100 "100 message"
      it "insert message" $ do
        insert log4 tree1 `shouldBe`
          Node (Node Leaf log2 Leaf) log1 (Node Leaf log3 (Node Leaf log4 Leaf))
      it "insert into Leaf" $ do
        insert log1 Leaf `shouldBe` Node Leaf log1 Leaf
      it "insert into log1 node" $ do
        insert log2 (insert log1 Leaf) `shouldBe`
          Node (Node Leaf log2 Leaf) log1 Leaf

      -- Test the build function the same as buildTree
      let tree2 = build [log1,log2,Unknown "uhoh",log3]
      it "build simple example" $ do
        tree2 `shouldBe` Node (Node Leaf log2 (Node Leaf log1 Leaf)) log3 Leaf
      let tree3 = build [log1,log2,Unknown "uhoh"]
      it "build simple example - two nodes" $ do
        tree3 `shouldBe` Node Leaf log2 (Node Leaf log1 Leaf)
      let tree4 = build [log1,Unknown "uhoh"]
      it "build simple example - one node" $ do
        tree4 `shouldBe` Node Leaf log1 Leaf
      let tree5 = build [log2,log1,Unknown "uhoh"]
      it "build simple example - two nodes again" $ do
        tree5 `shouldBe` Node (Node Leaf log2 Leaf) log1 Leaf

    describe "Get TimeStamp functions" $ do
      let logMsg1 = LogMessage Info 123 "hello"
      let tree1 = Node Leaf logMsg1 Leaf
      it "getMessageTreeTimeStamp simple example" $ do
        getMessageTreeTimeStamp tree1 `shouldBe` Just 123
      it "getLogMessageTimeStamp simple example" $ do
        getLogMessageTimeStamp logMsg1 `shouldBe` Just 123

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
