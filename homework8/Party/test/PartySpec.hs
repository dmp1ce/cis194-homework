module PartySpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

import Employee
import Party (glCons)

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
--      prop "ourAdd is commutative" $ \x y ->
--        ourAdd x y `shouldBe` ourAdd y x
