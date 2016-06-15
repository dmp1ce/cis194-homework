module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib  ( ourAdd
            , foldTree
            , Tree (Node,Leaf)
            , isTreeBalanced
            )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    prop "ourAdd is commutative" $ \x y ->
      ourAdd x y `shouldBe` ourAdd y x
    describe "foldTree" $ do
      let elementList = "A"
      let tree = foldTree elementList
      it "foldTree works" $ do
        tree `shouldBe` Node 0 Leaf 'A' Leaf
      it "foldTree is balanced" $ do
        isTreeBalanced tree `shouldBe` True

      let elementList2 = "ABCD"
      let tree2 = foldTree elementList2
      it "foldTree2 works" $ do
        tree2 `shouldBe` Node 2
                          (Node 1 (Node 0 Leaf 'A' Leaf) 'C' Leaf) 
                          'D'
                          (Node 0 Leaf 'B' Leaf)
      it "foldTree2 is balanced" $ do
        isTreeBalanced tree2 `shouldBe` True

-- Probably should also check that the so-called height value is correct with
-- another function, but I don't feel like doing that right now.
