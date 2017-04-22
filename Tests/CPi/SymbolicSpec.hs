module CPi.SymbolicSpec (spec) where

import Test.Hspec
-- import Test.QuickCheck
import qualified Data.Map as M
import Data.Either

import CPi.Symbolic

spec :: SpecWith ()
spec = do
  describe "Expr" $ do
    it "implements +" $
      var "a" + val 2 `shouldBe` var "a" `Sum` val 2
    it "implements -" $
      var "a" - val 2 `shouldBe` var "a" `Sum` (val (-1) `Prod` val 2)
    it "implements *" $
      var "a" * val 2 `shouldBe` var "a" `Prod` val 2
  describe "freeVars" $ do
    it "knows constants have no free vars" $
      freeVars (val 2) `shouldBe` []
    it "finds the free variables of a single variable" $
      freeVars (var "x") `shouldBe` ["x"]
    it "finds the free variables of a sum" $
      freeVars (var "x" + var "y" + var "z") `shouldBe` ["x", "y", "z"]
    it "finds the free variables of a product" $
      freeVars (var "x" * var "y" * var "z") `shouldBe` ["x", "y", "z"]
    it "finds free variables in deeper expressions" $
      freeVars (var "x" * (val 1 + tan(var "y" + var "z")))
        `shouldBe` ["x", "y", "z"]
  describe "eval" $ do
    let env = M.fromList [("x", 1.0), ("y", 2.0), ("z", 3.0)]
    it "evaluates a constant atom" $
      eval env (Const 2) `shouldBe` Right 2
    it "evaluates a variable atom" $
      eval env (Var "x") `shouldBe` Right 1
    it "gives error on evaluating missing variables in atoms" $
      eval env (Var "w") `shouldSatisfy` isLeft
    it "evaluates a constant" $
      eval env (val 2) `shouldBe` Right 2
    it "evaluates a variable" $
      eval env (var "x") `shouldBe` Right 1
    it "gives error on evaluating missing variables in expressions" $
      eval env (var "w") `shouldSatisfy` isLeft
    it "can sum two constants" $
      eval env (val 2 + val 4) `shouldBe` Right 6
    it "can sum two variables" $
      eval env (var "x" + var "y") `shouldBe` Right 3
    it "can add constants to variables" $
      eval env (var "x" + val 5) `shouldBe` Right 6
    it "propogates errors from sums" $
      eval env (val 2 + var "w") `shouldSatisfy` isLeft
