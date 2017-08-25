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
  describe "simplify" $ do
    it "disregards constant left multiples" $
      simplify (val 1 * var "a") `shouldBe` var "a"
    it "disregards constant right multiples" $
      simplify (var "a" * val 1) `shouldBe` var "a"
    it "cancels zero left multiples" $
      simplify (val 0 * var "a") `shouldBe` val 0
    it "cancels zero right multiples" $
      simplify (var "a" * val 0) `shouldBe` val 0
    it "multiplies constants" $
      simplify(val 2 * val 3) `shouldBe` val 6
    it "adds constants" $
      simplify(val 2 + val 3) `shouldBe` val 5
    it "cancels exp log" $
      simplify(exp (log (var "x"))) `shouldBe` var "x"
    it "cancels log exp" $
      simplify(log (exp (var "x"))) `shouldBe` var "x"
    it "brings log outside left prod" $
      simplify (var "a" * log(var "x")) `shouldBe` log (var "x" ** var "a")
    it "brings log outside right prod" $
      simplify (log(var "x") * var "a") `shouldBe` log (var "x" ** var "a")
    it "combines powers" $
      simplify((var "x" ** var "a") * (var "x" ** var "b"))
        `shouldBe` var "x" ** (var "a" + var "b")
    it "brings products outside of powers" $
      simplify((var "x" * var "y") ** var "a")
        `shouldBe` (var "x" ** var "a") * (var "y" ** var "a")
    it "combines double powers" $
      simplify((var "x" ** var "a") ** var "b")
        `shouldBe` var "x" ** (var "a" * var "b")
    it "can fully simplify complex expression with exp log" $
      simplify(var "x" * exp(val (-1.0) * log (val 1.0 * var "x")))
        `shouldBe` val 1.0
    it "simplifies fractions" $
      simplify (var "x" / var "x") `shouldBe` val 1.0
    it "can simplify separated fractions" $
      simplify (var "x" / (var "x" + var "y")
              + var "y" / (var "x" + var "y"))
        `shouldBe`val 1.0
    it "can bring coefficients inside fractions" $
      simplify (var "z" * (var "x" / var "y"))
        `shouldBe` simplify ((var "z" * var "x") / var "y")
    it "can simplify separated fractions with coefficients" $
      simplify (2.0 * (var "x" / (var "x" + var "y"))
              + 2.0 * (var "y" / (var "x" + var "y")))
        `shouldBe` val 2.0
    it "should not simplify this expr to zero" $
      simplify (Prod (Prod (Prod (Atom (Const 1.0)) (Atom (Const 2.0))) (Sum (Atom (Const 0.0)) (Abs (Prod (Prod (Prod (Atom (Const 1.0)) (Atom (Const 1.0))) (Prod (Prod (Atom (Const 1.0)) (Atom (Const 1.0))) (Atom (Const 1.0)))) (Atom (Var "[S]")))))) (Sum (Atom (Const 0.0)) (Abs (Prod (Prod (Prod (Atom (Const 1.0)) (Atom (Const 1.0))) (Prod (Prod (Atom (Const 1.0)) (Atom (Const 1.0))) (Atom (Const 1.0)))) (Atom (Var "[E]"))))))
        `shouldNotBe` val 0.0
    it "should not simplify this simpler expr to zero" $
      simplify (Prod (val 2.0) (Sum (val 0.0) (val 3.0)))
        `shouldBe` val 6.0
