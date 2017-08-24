module CPi.VectorSpec (spec) where

import Prelude hiding ((*>))

import Test.Hspec
import CPi.Vector
import CPi.Symbolic
import CPi.Base ()

spec :: SpecWith ()
spec = do
  describe "norm" $ do
    it "finds the l1 norm of 2.0<1| -3.0<2| + 1.5<3|" $
      norm (2.0 |> vect (1::Integer) +> (-3.0) |> vect 2 +> 1.5 |> vect 3) `shouldBe` (6.5::Double)
    it "finds the l1 norm of a symbolic vector" $
      simplify (norm $ var "a" |> vect (1::Integer) +> var "b" |> vect 2
                                +> var "c" |> vect 3)
      `shouldBe` abs(var "a") + abs(var "b") + abs(var "c")
    -- it "should only be zero for Nil" $
    --   property $ \x -> normalForm x == Nil || partial enzymeDefs (Mixture [(4.0, x)]) /= vectZero
  describe "multilinear" $ do
    it "extends a simple function to a multilinear one" $
      let f :: [Integer] -> Vect (Tensor Integer Integer) Double
          f [x, y] = vect ((x + 1) :* (y + 1))
          f _ = error "partially defined"
      in multilinear f [3 |> vect 1 +> 4 |> vect 2, 5 |> vect 6 +> 7 |> vect 8]
        `shouldBe` 15 |> vect (2 :* 7) +> 21 |> vect (2 :* 9)
          +> 20 |> vect (3 :* 7) +> 28 |> vect (3 :* 9)
    it "gives the correct result on basis elements" $
      let f :: [Integer] -> Vect (Tensor Integer Integer) Double
          f [x, y] = vect ((x + 1) :* (y + 1))
          f _ = error "partially defined"
      in multilinear f [vect 1, vect 6] `shouldBe` vect (2 :* 7)
    it "has the linear extension as a special case" $
      let f :: [Integer] -> Vect Integer Double
          f [x] = vect (x + 1)
          f _ = error "partially defined"
      in multilinear f [3 |> vect 1 +> 2 |> vect 6]
        `shouldBe` 3 |> vect 2 +> 2 |> vect 7
    it "gives linearity in first argument" $
      let f :: [Integer] -> Vect (Tensor Integer Integer) Double
          f [x, y] = vect ((x + 1) :* (y + 1))
          f _ = error "partially defined"
      in multilinear f [3 |> vect 1 +> 4 |> vect 2, vect 6]
        `shouldBe` 3 |> vect (2 :* 7) +> 4 |> vect (3 :* 7)
    it "gives linearity in second argument" $
      let f :: [Integer] -> Vect (Tensor Integer Integer) Double
          f [x, y] = vect ((x + 1) :* (y + 1))
          f _ = error "partially defined"
      in multilinear f [vect 6, 3 |> vect 1 +> 4 |> vect 2]
        `shouldBe` 3 |> vect (7 :* 2) +> 4 |> vect (7 :* 3)
