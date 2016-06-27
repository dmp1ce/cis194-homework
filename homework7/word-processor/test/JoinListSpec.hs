module JoinListSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Monoid

import Sized
import Scrabble
import JoinList (
                  (+++)
                , JoinList (Empty, Single, Append)
                , (!!?)
                , jlToList
                , indexJ
                , dropJ
                , ValidJoinList (ValidJoinList)
                , takeJ
                , scoreLine
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
      prop "indexJ == jlToList - QuickCheck" $ \i_ (ValidJoinList jl_) ->
        indexJ i_ (jl_ :: JoinList Size Char) `shouldBe` (jlToList jl_ !!? i_)
    context "dropJ" $ do
      it "dropJ == jlToList - Empty" $ do
        let jl  = (Empty :: JoinList Size a)
        let n   = 1
        jlToList ((dropJ n jl) :: JoinList Size Int)
          `shouldBe`
            drop n ((jlToList jl))
      it "dropJ == jlToList - left greater than n" $ do
        let jl  = (Append
                    3
                    (Append 2 (Single (Size 1) 'a') (Single (Size 1) 'b'))
                    (Single (Size 1) 'c'))
        let n   = 1
        jlToList (dropJ n jl)
          `shouldBe`
            drop n (jlToList jl)
      prop "dropJ == jlToList - QuickCheck" $ \i_ (ValidJoinList jl_) ->
        jlToList (dropJ i_ (jl_ :: JoinList Size Char)) == drop i_ (jlToList jl_)
    context "dropJ" $ do
      prop "takeJ == jlToList - QuickCheck" $ \i_ (ValidJoinList jl_) ->
        jlToList (takeJ i_ (jl_ :: JoinList Size Char)) == take i_ (jlToList jl_)
    context "Scrabble" $ do
      it "Scrabble example 1" $ do
        let expected  = Append (Score 23)
                        (Single (Score 9) "yay ")
                        (Single (Score 14) "haskell!")
        scoreLine "yay " +++ scoreLine "haskell!" `shouldBe` expected
