module LibSpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

import ExprT
import Lib  ( eval
            , evalStr
            , Expr (lit,mul,add)
            , testExp
            , MinMax (MinMax)
            , Mod7 (Mod7)
            )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
      it "eval example 1" $ do 
        eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
      it "evalStr example 1" $ do 
        evalStr "(2+3)*4" `shouldBe`
          Just (eval $ Mul (Add (Lit 2) (Lit 3)) (Lit 4))
      it "evalStr example 2" $ do 
        evalStr "2+3*4" `shouldBe`
          Just (eval $ Add (Lit 2) (Mul (Lit 3) (Lit 4)))
      it "evalStr example 3" $ do 
        evalStr "2+3*" `shouldBe`
          Nothing
      it "Expr class equals ExprT expression" $ do
        mul (add (lit 2) (lit 3)) (lit 4) `shouldBe`
          Mul (Add (Lit 2) (Lit 3)) (Lit 4)
      it "Exercise 4 - Integer test" $ do
        (testExp :: Maybe Integer) `shouldBe` Just (-7)
      it "Exercise 4 - Bool test" $ do
        (testExp :: Maybe Bool) `shouldBe` Just (True)
      it "Exercise 4 - MinMax test" $ do
        (testExp :: Maybe MinMax) `shouldBe` Just (MinMax 5)
      it "Exercise 4 - Mod7 test" $ do
        (testExp :: Maybe Mod7) `shouldBe` Just (Mod7 0)

  --  it "works" $ do
  --    True `shouldBe` True
  --  prop "ourAdd is commutative" $ \x y ->
  --    ourAdd x y `shouldBe` ourAdd y x
