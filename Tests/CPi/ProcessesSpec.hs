module CPi.ProcessesSpec (spec) where

import Prelude hiding ((*>))

import Test.Hspec
import CPi.AST
import CPi.Base
import CPi.Processes
-- import Data.Map (Map)
import qualified Data.Map as M
import CPi.Vector
import CPi.Symbolic

massAction :: RateLawFamily
massAction [k] xs = k * product xs
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

enzymeS'bound :: Species
enzymeS'bound = mkSum [(Located "r" 0, mkAbsBase (Def "S'" [] [])),
                       (Located "p" 0, mkAbsBase (Def "P" [] []))]

enzymeS :: SpeciesDefinition
enzymeS = SpeciesDef [] [] $ mkSum [(Unlocated "s", mkAbs 0 enzymeSbound)]

enzymeS' :: SpeciesDefinition
enzymeS' = SpeciesDef [] [] $ mkSum [(Unlocated "s", mkAbs 0 enzymeS'bound)]

enzymeDefs :: Env
enzymeDefs = M.fromList [("E", enzymeE), ("S", enzymeS), ("P", enzymeP)]

enzymeDefs2 :: Env
enzymeDefs2 = M.fromList [("E", enzymeE), ("S", enzymeS), ("S'", enzymeS'), ("P", enzymeP)]

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

partialEnzymeMM :: D'
partialEnzymeMM
  =  var "[S]" |> vect (Def "S" [] [] :* mkAbsBase (Def "P" [] [])
                                      :* [Unlocated "s"])
  +> var "[P]" |> vect (Def "P" [] [] :* mkAbsBase (Def "P" [] [])
                                      :* [Unlocated "p"])
  +> var "[E]" |> vect (Def "E" [] [] :* mkAbsBase (Def "E" [] [])
                                      :* [Unlocated "e"])

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
  describe "conc" $ do
    it "finds the concentration of a site s in partial E||S" $
      conc [Unlocated "s"] (partialS +> partialE) `shouldBe` 3.0
    it "finds the concentration of a site e in partial E||S" $
      conc [Unlocated "e"] (partialS +> partialE) `shouldBe` 2.0
    it "finds the concentration of site s from symbolic MM enzyme" $
      simplify (conc [Unlocated "s"] partialEnzymeMM) `shouldBe` abs(var "[S]")
  describe "direct" $ do
    it "finds the direction of E||S at site s" $
      direct [Unlocated "s"] (partialS +> partialE)
        `shouldBe` vect (Def "S" [] []) *> vect (simplify $ mkAbs 0 enzymeSbound)
    it "finds the direction of E||S at site e" $
      direct [Unlocated "e"] (partialS +> partialE)
        `shouldBe` vect (Def "E" [] []) *> vect (simplify $ mkAbs 0 enzymeEbound)
    it "finds the direction of s in symbolic MM interactions" $
      simplify (direct [Unlocated "s"] partialEnzymeMM)
        `shouldBe` simplify (vect (Def "S" [] [] :* mkAbsBase (Def "P" [] [])))
  describe "hide" $ do
    it "hides some sites in an interaction vector" $
      hide [[Unlocated "e"]] (partialS +> partialE) `shouldBe` partialS
    it "hides a site in the symbolic interaction vector for MM enzymes" $
      toList (simplify (hide [[Unlocated "e"]] partialEnzymeMM))
      `shouldBe`
      toList (simplify (var "[S]" |> vect (Def "S" [] [] :* mkAbsBase (Def "P" [] [])
                                                 :* [Unlocated "s"])
             +> var "[P]" |> vect (Def "P" [] [] :* mkAbsBase (Def "P" [] [])
                                                 :* [Unlocated "p"])))
  describe "react" $ do
    it "can react an enzyme and a substrate" $
      react [(vect (Def "S" [] []) :: ProcessVect Conc) *> vect (mkAbs 0 enzymeSbound),
             vect (Def "E" [] []) *> vect (mkAbs 0 enzymeEbound)]
        `shouldBe` vect (simplify $ new [0] (enzymeSbound <|>  enzymeEbound))
          +> (-1.0) |> vect (Def "S" [] []) +> (-1.0) |> vect (Def "E" [] [])
    it "can react a symbolic enzyme and substrate" $
      simplify (react [(var "[S]" / (var "[S]" + var "[S']"))
               |> vect (Def "S" [] []) *> vect (mkAbs 0 enzymeSbound)
          +> (var "[S']" / (var "[S]" + var "[S']"))
               |> vect (Def "S'" [] []) *> vect (mkAbs 0 enzymeS'bound),
              val 1 |> vect (Def "E" [] []) *> vect (mkAbs 0 enzymeEbound)])
        `shouldBe`
        simplify (
        (var "[S]" / (var "[S]" + var "[S']"))
          |> (vect (new [0] (enzymeSbound <|> enzymeEbound))
          +> val (-1.0) |> vect (Def "S" [] []))
     +> (var "[S']" / (var "[S]" + var "[S']"))
          |> (vect (new [0] (enzymeS'bound <|> enzymeEbound))
          +> val (-1.0) |> vect (Def "S'" [] []))
     +> val (-1.0) |> vect (Def "E" [] []))
    it "can react a symbolic enzyme and substrate, upto pretty printing" $
      pretty (simplify (react [(var "[S]" / (var "[S]" + var "[S']"))
               |> vect (Def "S" [] []) *> vect (mkAbs 0 enzymeSbound)
          +> (var "[S']" / (var "[S]" + var "[S']"))
               |> vect (Def "S'" [] []) *> vect (mkAbs 0 enzymeS'bound),
              val 1 |> vect (Def "E" [] []) *> vect (mkAbs 0 enzymeEbound)]))
        `shouldBe`
        pretty (simplify (
        (var "[S]" / (var "[S]" + var "[S']"))
          |> (vect (new [0] (enzymeSbound <|> enzymeEbound))
          +> val (-1.0) |> vect (Def "S" [] []))
     +> (var "[S']" / (var "[S]" + var "[S']"))
          |> (vect (new [0] (enzymeS'bound <|> enzymeEbound))
          +> val (-1.0) |> vect (Def "S'" [] []))
     +> val (-1.0) |> vect (Def "E" [] [])))
  describe "actions" $
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
