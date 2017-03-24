module CPi.ProcessesSpec (spec) where

import Prelude hiding ((*>))

import Test.Hspec
import CPi.AST
import CPi.Processes
-- import Data.Map (Map)
import qualified Data.Map as M
import QuantumVector

massAction :: RateLawFamily
massAction [k] xs = k * (product xs)
massAction _ _ = error "Mass action takes only one parameter"

enzymeAffinityNetwork :: AffinityNetwork
enzymeAffinityNetwork =
  [ Affinity (massAction [1]) [[Unlocated "e"], [Unlocated "s"]]
  , Affinity (massAction [2]) [[Unlocated "x", Unlocated "r"]]
  , Affinity (massAction [3]) [[Unlocated "x", Unlocated "p"]] ]

enzymeEbound :: Species
enzymeEbound = Sum [(Located "x" 0, AbsBase (Def "E" [] []))]

enzymeE :: Definition
enzymeE = SpeciesDef [] [] $ Sum [(Unlocated "e", Abs 0 enzymeEbound)]

enzymeP :: Definition
enzymeP = SpeciesDef [] [] $ Sum [(Unlocated "d", AbsBase (Def "P" [] []))]

enzymeSbound :: Species
enzymeSbound = (Sum [(Located "r" 0, AbsBase (Def "S" [] [])),
                     (Located "p" 0, AbsBase (Def "P" [] []))])

enzymeS :: Definition
enzymeS = SpeciesDef [] [] $ Sum [(Unlocated "s", Abs 0 enzymeSbound)]

enzymeDefs :: Env
enzymeDefs = M.fromList [("E", enzymeE), ("S", enzymeS), ("P", enzymeP)]

-- enzymeES :: Species
-- enzymeES = Def "E" [] [] <|> Def "S" [] []

-- enzymeC :: Species
-- enzymeC = new [0] $ enzymeEbound <|> enzymeSbound

partialS :: D
partialS = 3.0 |> Ket (Def "S" [] [])
           *> Ket (simplify $ Abs 0 enzymeSbound) *> Ket [Unlocated "s"]

partialE :: D
partialE = 2.0 |> Ket (Def "E" [] [])
           *> Ket (simplify $ Abs 0 enzymeEbound) *> Ket [Unlocated "e"]

spec :: SpecWith ()
spec = do
  describe "partial" $ do
    it "finds no partial interactions for the empty process" $
      partial M.empty (Mixture []) `shouldBe` KetZero
    it "finds no partial interactions for the empty species" $
      partial M.empty (Mixture [(0.0, Nil)]) `shouldBe` KetZero
    it "finds the partial interactions for E" $
      partial enzymeDefs (Mixture [(2.0, Def "E" [] [])])
        `shouldBe` partialE
    it "finds the partial interactions for S" $
      partial enzymeDefs (Mixture [(3.0, Def "S" [] [])])
        `shouldBe` partialS
    it "finds the partial interactions for a mixture of E and S" $
      partial enzymeDefs (Mixture [(2.0, Def "E" [] []),
                                   (3.0, Def "S" [] [])])
        `shouldBe` partialS +> partialE
    it "should return no interactions after hiding" $
      partial enzymeDefs
              (React enzymeAffinityNetwork
                    (Mixture [(2.0, Def "E" [] []),
                              (3.0, Def "S" [] [])]))
        `shouldBe` KetZero
  describe "norm1" $ do
    it "finds the l1 norm of 2.0<1| -3.0<2| + 1.5<3|" $
      norm1 (2.0 |> Ket (1::Integer) +> (-3.0) |> Ket 2 +> 1.5 |> Ket 3) `shouldBe` 6.5
  describe "conc" $ do
    it "finds the concentration of a site s in partial E||S" $
      conc [Unlocated "s"] (partialS +> partialE) `shouldBe` 3.0
    it "finds the concentration of a site e in partial E||S" $
      conc [Unlocated "e"] (partialS +> partialE) `shouldBe` 2.0
  describe "direct" $ do
    it "finds the direction of E||S at site s" $
      direct [Unlocated "s"] (partialS +> partialE)
        `shouldBe` Ket (Def "S" [] []) *> Ket (simplify $ Abs 0 enzymeSbound)
    it "finds the direction of E||S at site e" $
      direct [Unlocated "e"] (partialS +> partialE)
        `shouldBe` Ket (Def "E" [] []) *> Ket (simplify $ Abs 0 enzymeEbound)
  describe "hide" $ do
    it "hides some sites in an interaction vector" $
      hide [[Unlocated "e"]] (partialS +> partialE) `shouldBe` partialS
  describe "multilinear" $ do
    it "extends a simple function to a multilinear one" $
      let f :: [Ket Integer] -> Ket (Tuple Integer Integer)
          f [Ket x, Ket y] = Ket ((x + 1) :* (y + 1))
          f _ = error "partially defined"
      in multilinear f [3 |> Ket 1 +> 4 |> Ket 2, 5 |> Ket 6 +> 7 |> Ket 8]
        `shouldBe` 15 |> Ket (2 :* 7) +> 21 |> Ket (2 :* 9)
          +> 20 |> Ket (3 :* 7) +> 28 |> Ket (3 :* 9)
    it "gives the correct result on basis elements" $
      let f :: [Ket Integer] -> Ket (Tuple Integer Integer)
          f [Ket x, Ket y] = Ket ((x + 1) :* (y + 1))
          f _ = error "partially defined"
      in multilinear f [Ket 1, Ket 6] `shouldBe` Ket (2 :* 7)
    it "has the linear extension as a special case" $
      let f :: [Ket Integer] -> Ket Integer
          f [Ket x] = Ket (x + 1)
          f _ = error "partially defined"
      in multilinear f [3 |> Ket 1 +> 2 |> Ket 6]
        `shouldBe` 3 |> Ket 2 +> 2 |> Ket 7
    it "gives linearity in first argument" $
      let f :: [Ket Integer] -> Ket (Tuple Integer Integer)
          f [Ket x, Ket y] = Ket ((x + 1) :* (y + 1))
          f _ = error "partially defined"
      in multilinear f [3 |> Ket 1 +> 4 |> Ket 2, Ket 6]
        `shouldBe` 3 |> Ket (2 :* 7) +> 4 |> Ket (3 :* 7)
    it "gives linearity in second argument" $
      let f :: [Ket Integer] -> Ket (Tuple Integer Integer)
          f [Ket x, Ket y] = Ket ((x + 1) :* (y + 1))
          f _ = error "partially defined"
      in multilinear f [Ket 6, 3 |> Ket 1 +> 4 |> Ket 2]
        `shouldBe` 3 |> Ket (7 :* 2) +> 4 |> Ket (7 :* 3)
  describe "react" $ do
    it "can react an enzyme and a substrate" $
      react [Ket (Def "S" [] []) *> Ket (Abs 0 enzymeSbound),
             Ket (Def "E" [] []) *> Ket (Abs 0 enzymeEbound)]
        `shouldBe` Ket (simplify $ new [0] (enzymeSbound <|>  enzymeEbound))
          +> (-1.0) |> Ket (Def "S" [] []) +> (-1.0) |> Ket (Def "E" [] [])
  describe "actions" $ do
    it "finds the actions when reacting substrate and enzyme" $
      actions enzymeAffinityNetwork (partialS +> partialE)
        `shouldBe` 6.0 |> Ket (simplify $ new [0] (enzymeSbound <|>  enzymeEbound))
          +> (-6.0) |> Ket (Def "S" [] []) +> (-6.0) |> Ket (Def "E" [] [])
  describe "dP/dt" $ do
    it "gives correct actions for reacting substrate and enzyme" $
      dPdt enzymeDefs (React enzymeAffinityNetwork
        (Mixture [(3.0, Def "S" [] []), (2.0, Def "E" [] [])]))
        `shouldBe` 6.0 |> Ket (simplify $ new [0] (enzymeSbound <|>  enzymeEbound))
          +> (-6.0) |> Ket (Def "S" [] []) +> (-6.0) |> Ket (Def "E" [] [])
