module PartySpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

import Employee
import Data.Tree
import Party (glCons, treeFold, nextLevel, maxFun)

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

    context "nextLevel" $ do
      let bob       = Emp "Bob" 1
      let gl        = GL [bob] (empFun bob)
      let glNoBoss  = GL [] 0
      it "Empty sub trees" $ do
        nextLevel bob [] `shouldBe` (gl, glNoBoss)
      let dave = Emp "Dave" 1
      let mell = Emp "Mell" 2
      let will = Emp "Will" 10
      let glSub       = GL [dave,will] 11
      let glSubNoBoss = GL [mell] 2
      it "One subtree" $ do
        nextLevel bob [(glSub, glSubNoBoss)] `shouldBe`
          (GL [bob,mell] 3, GL [dave,will] 11)
      let marc = Emp "Marc" 2
      let donna = Emp "Donna" 1
      let lenna = Emp "Lenna" 5
      let glSub2          = GL [marc] 2
      let glSub2NoBoss    = GL [donna, lenna] 6
      it "Two subtrees" $ do
        nextLevel bob [(glSub, glSubNoBoss),(glSub2,glSub2NoBoss)] `shouldBe`
          (GL [bob,mell,donna,lenna] 9, GL [dave,will,marc] 13)

    context "maxFun" $ do
      let treeOfOne = Node (Emp "Dave" 3) []
      it "Tree of one" $ do
        maxFun treeOfOne `shouldBe` GL [(Emp "Dave" 3)] 3
      let treeOfTwo = Node (Emp "Mell" 2) [treeOfOne]
      it "Tree of two" $ do
        maxFun treeOfTwo `shouldBe` GL [(Emp "Dave" 3)] 3

--      prop "ourAdd is commutative" $ \x y ->
--        ourAdd x y `shouldBe` ourAdd y x
