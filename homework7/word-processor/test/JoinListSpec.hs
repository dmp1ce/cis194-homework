module JoinListSpec where

import Test.Hspec
--import Test.Hspec.QuickCheck
import Data.Monoid

import JoinList (
                  (+++)
                , JoinList (Empty, Single, Append)
                )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "JoinList tests" $ do
    let a     = Single (Product (3::Integer)) 'a'
    let e     = Single (Product 2) 'e'
    let ae    = Append (Product 6) a e
    let aea   = Append (Product 18) ae a
    it "+++ JoinList Singles" $ do
      a +++ e `shouldBe` ae
    it "+++ JoinList Empty" $ do
      a +++ Empty `shouldBe` a
    it "+++ JoinList Append" $ do
      ae +++ a `shouldBe` aea
--    it "works" $ do
--      True `shouldBe` True
--    prop "ourAdd is commutative" $ \x y ->
--      ourAdd x y `shouldBe` ourAdd y x
