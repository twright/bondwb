module BondCalculus.SymbolicSpec (spec) where

import Test.Hspec
-- import Test.QuickCheck
import qualified Data.Map as M
import Data.Either
import Data.Bifunctor
import BondCalculus.ODEExtraction (sympyExpr)

import qualified BondCalculus.Base as Base
import BondCalculus.Symbolic hiding (var)
import qualified BondCalculus.Symbolic as Symb

var :: String -> SymbolicExpr Double
var = Symb.var
val :: Double -> SymbolicExpr Double
val = Base.val

spec :: SpecWith ()
spec = do
  describe "Expr" $ do
    it "implements +" $
      var "a" + val 2 `shouldBe` var "a" `Sum` val 2
    it "implements -" $
      var "a" - val 2 `shouldBe` var "a" `Sum` val (-2)
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
    let env = M.fromList [("x", 1.0 :: Double), ("y", 2.0), ("z", 3.0)]
        envsymb = M.fromList [("x", var "a"), ("y", var "b"), ("z", val 3.0)]
    it "evaluates a constant atom" $
      eval env (val 2) `shouldBe` Right 2
    it "evaluates a variable atom" $
      eval env (var "x") `shouldBe` Right 1
    it "gives error on evaluating missing variables in atoms" $
      eval env (var "w") `shouldSatisfy` isLeft
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
    it "evaluates using a symbolic environment" $
      second simplify (eval envsymb ((var "x" + var "y") * var "z"))
        `shouldBe`
        Right (simplify $ val 3.0 * (var "a" + var "b"))
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
    it "can take constants inside fractions coefficients" $
      simplify (2.0 * (var "x" / (var "x" + var "y")))
        `shouldBe`
        ((var "x" * 2.0) / (var "x" + var "y"))
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
    it "should handle dividing fractions" $
      simplify ((var "a" / var "b") / var "c")
        `shouldBe` simplify(var "a" / (var "b" * var "c"))
    it "should cancel fractions" $
      simplify ((var "a" * var "b" * var "c")/(var "d" * var "a" * var "c"))
        `shouldBe` var "b" / var "d"
    it "can simplify complex fraction (from ping-pong law)" $
      simplify ((var "E" * (var "A" * var "B")) / ((2.0 * var "A") * var "E" + (2.0 * var "B") * var "E" + (var "A" * var "B") * var "E"))
        `shouldBe`
        simplify ((var "A" * var "B") / (2.0 * var "A" + 2.0 * var "B" + var "A" * var "B"))
    it "simplifies dividing by an even power" $
      simplify(var "a"/(var "b" * var "a" ** 2))
        `shouldBe` val 1 / (var "a" * var "b")
    it "simplifies dividing by an odd power" $
      simplify(var "a"/(var "b" * var "a" ** 3))
        `shouldBe` val 1 / (var "b" * var "a" ** 2)
    it "simplifies multiplications into powers" $
      simplify(var "a" * var "a" * var "a")
        `shouldBe` var "a" ** 3
    it "simplifies multiplications of powers into powers" $
      simplify(var "a" ** 2 * var "a" ** 3)
        `shouldBe` var "a" ** 5
    it "simplifies fractions with multiplication" $
      simplify( (var "a" + var "b" + var "c")*(var "d" * var "f" + var "e" * var "f") / (var "a" + var "b" + var "c") / var "f" )
        `shouldBe` var "d" + var "e"
    xit "simplifies repressilator Cl expression" $
      sympyExpr (M.fromList [("x" ++ show i, "x" ++ show i) | i <- [0..5]]) (simplify (((val 10.0 * (var "x5" + var "x0" + var "x1")) * (var "x1" * val (-1.0))) / (var "x5" + var "x0" + var "x1") + (val 10.0 * (var "x3" + var "x4" + var "x2") * (var "x4")) / (var "x4" + var "x3" + var "x2")))
        `shouldBe` Just "x"
    it "does not mess up Goldbeater-Koshland denominator" $
        let expr = (var "a0" + var "a1"
                    + (var "x1") * (val (-1))
                    + (var "a2") * (var "x1")
                    + ((val (-1)) * ((var "a3") * ((var "a4") * (var "x1")))
                    + (val (-1)) * ((var "a5")
                       * ((var "x1") * ((val 4.0) * ((var "x1") * (val (-1))))))
                    + (var "a6" + var "a7"
                       + (var "x1") * (val (-1))
                       +  (var "a8") * (var "x1")) ** (val 2.0)) ** (val 0.5))
        in simplify expr `shouldBe` expr
  describe "factors" $ do
    it "finds factors of a product" $
      factors (var "a" * var "b" * var "c") `shouldBe`
        (1.0, [var "a", var "b", var "c"])
    it "finds common factors of a sum" $
      factors (var "a" * var "b" + var "c" * var "a") `shouldBe`
        (1.0, [var "a", var "b" + var "c"])
    it "finds common factors of a longer sum" $
      factors (var "a" * var "b" + var "c" * var "a" + var "a" * var "d") `shouldBe`
        (1.0, [var "a", var "b" + var "c" + var "d"])
    it "finds common factors from sum including coff" $
      factors (var "a" * val 2.0 + val 2.0 * var "b" * var "a")
        `shouldBe`
        (2.0, [var "a", val 1 + var "b"])
    it "finds common factors in product avoiding incomplete match" $
      factors ((var "a" + var "b")*var "a")
        `shouldBe`
        (1.0, [var "a", var "a" + var "b"])
    it "finds common factors in sum avoiding incomplete match" $
      factors (var "a" * var "b" + var "b" * var "c" + var "a" * var "c")
        `shouldBe`
        (1.0, [var "a" * var "b" + var "b" * var "c" + var "a" * var "c"])
    it "finds common factors in sum with repeated terms" $
      factors (var "C" * var "C" * 10 + var "T" * var "C" * 10)
        `shouldBe`
        (10, [var "C", var "C" + var "T"])
