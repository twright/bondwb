module CPi.ProcessesSpec (spec) where

import Prelude hiding ((*>))

import Test.Hspec
import Test.QuickCheck
import CPi.AST
import CPi.Processes
-- import Data.Map (Map)
import qualified Data.Map as M
import CPi.Vector

massAction :: RateLawFamily
massAction [k] xs = k * (product xs)
massAction _ _ = error "Mass action takes only one parameter"

enzymeAffinityNetwork :: ConcreteAffinityNetwork
enzymeAffinityNetwork =
  [ ConcreteAffinity (massAction [1]) [["e"], ["s"]]
  , ConcreteAffinity (massAction [2]) [["x", "r"]]
  , ConcreteAffinity (massAction [3]) [["x", "p"]] ]

enzymeEbound :: Species
enzymeEbound = mkSum [(Located "x" 0, mkAbsBase (Def "E" [] []))]

enzymeE :: SpeciesDefinition
enzymeE = SpeciesDef [] [] $ mkSum [(Unlocated "e", mkAbs 0 enzymeEbound)]

enzymeP :: SpeciesDefinition
enzymeP = SpeciesDef [] [] $ mkSum [(Unlocated "d", mkAbsBase (Def "P" [] []))]

enzymeSbound :: Species
enzymeSbound = mkSum [(Located "r" 0, mkAbsBase (Def "S" [] [])),
                     (Located "p" 0, mkAbsBase (Def "P" [] []))]

enzymeS :: SpeciesDefinition
enzymeS = SpeciesDef [] [] $ mkSum [(Unlocated "s", mkAbs 0 enzymeSbound)]

enzymeDefs :: Env
enzymeDefs = M.fromList [("E", enzymeE), ("S", enzymeS), ("P", enzymeP)]

-- enzymeES :: Species
-- enzymeES = Def "E" [] [] <|> Def "S" [] []

enzymeC :: Species
enzymeC = new [0] $ enzymeEbound <|> enzymeSbound

partialS :: D
partialS = 3.0 |> vect (Def "S" [] [])
           *> vect (simplify $ mkAbs 0 enzymeSbound) *> vect [Unlocated "s"]

partialE :: D
partialE = 2.0 |> vect (Def "E" [] [])
           *> vect (simplify $ mkAbs 0 enzymeEbound) *> vect [Unlocated "e"]

partialC :: D
partialC = vect (normalForm enzymeC
                :* normalForm (mkAbsBase (Def "E" [] [] <|> Def "S" [] []))
                :* [Unlocated "r", Unlocated "x"]) +>
             vect (normalForm enzymeC
                  :* normalForm (mkAbsBase (Def "E" [] [] <|> Def "P" [] []))
                  :* [Unlocated "p", Unlocated "x"]) +>
             vect (normalForm enzymeC
                  :* normalForm (mkAbsBase (Def "E" [] []
                                          <|> new [0] enzymeSbound))
                  :* [Unlocated "x"]) +>
             vect (normalForm enzymeC
                  :* normalForm (mkAbsBase (new [0] enzymeEbound
                                          <|> Def "S" [] []))
                  :* [Unlocated "r"]) +>
             vect (normalForm enzymeC
                  :* normalForm (mkAbsBase (new [0] enzymeEbound
                                          <|> Def "P" [] []))
                  :* [Unlocated "p"])

spec :: SpecWith ()
spec = do
  describe "partial" $ do
    it "finds no partial interactions for the empty process" $
      partial M.empty (Mixture []) `shouldBe` vectZero
    it "finds no partial interactions for the empty species" $
      partial M.empty (Mixture [(0.0, Nil)]) `shouldBe` vectZero
    it "finds the partial interactions for E" $
      partial enzymeDefs (Mixture [(2.0, Def "E" [] [])])
        `shouldBe` partialE
    it "finds the partial interactions for S" $
      partial enzymeDefs (Mixture [(3.0, Def "S" [] [])])
        `shouldBe` partialS
    it "finds the partial interactions for a mixture of E and S" $
        partial enzymeDefs (Mixture [(2.0, Def "E" [] []),
                                      (3.0, Def "S" [] [])])
           `shouldBe`(partialS +> partialE)
    it "should return no interactions after hiding" $
      partial enzymeDefs
              (React enzymeAffinityNetwork
                    (Mixture [(2.0, Def "E" [] []),
                              (3.0, Def "S" [] [])]))
        `shouldBe` vectZero
    it "finds the partial interaction for an enzyme" $
      partial enzymeDefs (Mixture [(4.0, enzymeC)]) `shouldBe` (4 |> partialC)
    -- it "should only be zero for Nil" $
    --   property $ \x -> normalForm x == Nil || partial enzymeDefs (Mixture [(4.0, x)]) /= vectZero
  describe "norm" $ do
    it "finds the l1 norm of 2.0<1| -3.0<2| + 1.5<3|" $
      norm (2.0 |> vect (1::Integer) +> (-3.0) |> vect 2 +> 1.5 |> vect 3) `shouldBe` (6.5::Double)
  describe "conc" $ do
    it "finds the concentration of a site s in partial E||S" $
      conc [Unlocated "s"] (partialS +> partialE) `shouldBe` 3.0
    it "finds the concentration of a site e in partial E||S" $
      conc [Unlocated "e"] (partialS +> partialE) `shouldBe` 2.0
  describe "direct" $ do
    it "finds the direction of E||S at site s" $
      direct [Unlocated "s"] (partialS +> partialE)
        `shouldBe` vect (Def "S" [] []) *> vect (simplify $ mkAbs 0 enzymeSbound)
    it "finds the direction of E||S at site e" $
      direct [Unlocated "e"] (partialS +> partialE)
        `shouldBe` vect (Def "E" [] []) *> vect (simplify $ mkAbs 0 enzymeEbound)
  describe "hide" $ do
    it "hides some sites in an interaction vector" $
      hide [[Unlocated "e"]] (partialS +> partialE) `shouldBe` partialS
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
  describe "react" $ do
    it "can react an enzyme and a substrate" $
      react [vect (Def "S" [] []) *> vect (mkAbs 0 enzymeSbound),
             vect (Def "E" [] []) *> vect (mkAbs 0 enzymeEbound)]
        `shouldBe` vect (simplify $ new [0] (enzymeSbound <|>  enzymeEbound))
          +> (-1.0) |> vect (Def "S" [] []) +> (-1.0) |> vect (Def "E" [] [])
  describe "actions" $ do
    it "finds the actions when reacting substrate and enzyme" $
      shouldBe
        (6.0 |> vect (simplify $ new [0] (enzymeSbound <|>  enzymeEbound))
          +> (-6.0) |> vect (Def "S" [] []) +> (-6.0) |> vect (Def "E" [] []))
        (actions enzymeAffinityNetwork (partialS +> partialE))
  describe "dPdt" $ do
    it "gives correct dPdt for reacting substrate and enzyme" $
      shouldBe
        (6.0 |> vect (simplify $ new [0] (enzymeSbound <|>  enzymeEbound))
          +> (-6.0) |> vect (Def "S" [] []) +> (-6.0) |> vect (Def "E" [] []))
        (dPdt enzymeDefs (React enzymeAffinityNetwork
          (Mixture [(3.0, Def "S" [] []), (2.0, Def "E" [] [])])))
    it "gives correct dPdt for enzyme substrate complex" $
      shouldBe
        (dPdt enzymeDefs (React enzymeAffinityNetwork (Mixture [(4.0, enzymeC)])))
        (12 |> vect (Def "P" [] []) +> 8 |> vect (Def "S" [] []) +> 20 |> vect (Def "E" [] []) +> (-20.0) |> vect (simplify enzymeC))
