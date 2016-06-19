module LibSpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

import Lib  ( fib
            , nats
            , streamToList
            , streamRepeat
            , interleaveStreams
            , ruler
            , x
            , fibs2
            , fibs3
            )

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
    context "Streams" $ do
      it "natural numbers" $ do
        take 3 (streamToList nats) `shouldBe` [0,1,2]
      it "interleave streams" $ do
        let s1 = streamRepeat (0 :: Integer)
        let s2 = streamRepeat (1 :: Integer)
        take 6 (streamToList $ interleaveStreams s1 s2) `shouldBe`
          [0,1,0,1,0,1]
      it "ruler" $ do
        --pending
        take 16 (streamToList ruler) `shouldBe`
          [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4]
    context "Fib stream" $ do
      it "x" $ do
        take 5 (streamToList x) `shouldBe` [0,1,0,0,0]
      it "fibs2 == fibs3" $ do
        take 20 fibs2 `shouldBe` take 20 (streamToList fibs3)
