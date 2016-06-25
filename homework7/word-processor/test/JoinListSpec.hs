module JoinListSpec where

import Test.Hspec
--import Test.Hspec.QuickCheck
import Data.Monoid

import Sized
import JoinList (
                  (+++)
                , JoinList (Empty, Single, Append)
                , (!!?)
                , jlToList
                , indexJ
                , dropJ
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
    context "indexJ" $ do
      let jl_empty  = (Empty :: JoinList Size a)
      let i   = 0
      let test_empty  = indexJ i jl_empty
      it "indexJ == jlToList (Empty)" $ do
        (test_empty :: Maybe (JoinList Size ()))
          `shouldBe` (Nothing :: Maybe (JoinList Size ()))
      let jl = Single (Size 0) "Hello"
      let test  = indexJ i jl
      it "indexJ == jlToList (Single)" $ do
        test `shouldBe` Just "Hello"
      it "indexJ == jlToList (Single) index out of bounds" $ do
        indexJ 1 jl `shouldBe` Nothing
      it "indexJ == jlToList (Single) index out of bounds negative" $ do
        indexJ (-1) jl `shouldBe` Nothing
      it "indexJ == jlToList - Ignores Empty" $ do
        let jl' = Append (Size 1) Empty (Single (Size 1) 'a')
        indexJ 0 jl' `shouldBe` jlToList jl' !!? 0
      -- Cannot use arbitrary right now because arbitrary produces invalid JoinLists
      --prop "indexJ == jlToList - QuickCheck" $ \i_ jl_ ->
      --  indexJ i_ (jl_ :: JoinList Size Char) `shouldBe` (jlToList jl_ !!? i_)
    context "dropJ" $ do
      it "dropJ == jlToList - Empty" $ do
        let jl  = (Empty :: JoinList Size a)
        let n   = 1
        jlToList ((dropJ n jl) :: JoinList Size Int)
          `shouldBe`
            drop n ((jlToList jl))
