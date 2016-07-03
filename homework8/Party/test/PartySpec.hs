module PartySpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

import Employee
import Data.Tree
import Party (glCons, treeFold)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Party" $ do
    context "GuestList" $ do
      let glEmpty = GL [] 0
      let bob = Emp { empName="Bob", empFun=1 }
      let glOnlyBob = GL [bob] 1
      it "Add guest to empty guest list" $ do
        bob `glCons` glEmpty `shouldBe` glOnlyBob
      let steve = Emp { empName="Steve", empFun=2 }
      let glBobSteve = GL [steve,bob] 3
      it "Add guest to guest list that bob is on" $ do
        steve `glCons` glOnlyBob `shouldBe` glBobSteve
    context "treeFold" $ do
      let myTree = Node "a" [Node "b" [], Node "c" [], Node "d" []]
      it "treeFold works with strings" $ do
        treeFold (\t l -> t ++ concat l) myTree
          `shouldBe` "abcd"
--      prop "ourAdd is commutative" $ \x y ->
--        ourAdd x y `shouldBe` ourAdd y x
