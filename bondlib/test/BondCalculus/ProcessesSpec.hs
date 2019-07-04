module BondCalculus.ProcessesSpec (spec) where

import Prelude hiding ((*>), (<>))

import Test.Hspec
import BondCalculus.AST
import BondCalculus.Base
import BondCalculus.Processes
-- import Data.Map (Map)
import qualified Data.Map as M
import BondCalculus.Vector
import BondCalculus.Examples

import BondCalculus.Symbolic hiding (var, val)
import qualified BondCalculus.Symbolic as Symb

var :: String -> SymbolicExpr
var = Symb.var
val :: Double -> SymbolicExpr
val = Symb.val

spec :: SpecWith ()
spec = do
  describe "partial" $ do
    it "finds no partial interactions for the empty process" $
      partial M.empty (Mixture []) `shouldBe` (vectZero :: InteractionVect Conc)
    it "finds no partial interactions for the empty species" $
      partial M.empty (Mixture [(0.0, Nil)]) `shouldBe` (vectZero :: InteractionVect Conc)
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
              (React affinityNetworkEnzyme
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
      direct [Unlocated "s"] partialEnzymeMM
        `shouldBe` (var "[S]" / abs (var "[S]")) |> vect (Def "S" [] [] :* mkAbsBase (Def "P" [] []))
  describe "hide" $ do
    it "hides some sites in an interaction vector" $
      hide [[Unlocated "e"]] (partialS +> partialE) `shouldBe` partialS
    it "hides a site in the symbolic interaction vector for MM enzymes" $
      hide [[Unlocated "e"]] partialEnzymeMM
      `shouldBe`
      var "[S]" |> vect (Def "S" [] [] :* mkAbsBase (Def "P" [] [])
                                                 :* [Unlocated "s"])
             +> var "[P]" |> vect (Def "P" [] [] :* mkAbsBase (Def "P" [] [])
                                                 :* [Unlocated "p"])
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
        simplify ((var "[S]" / (var "[S]" + var "[S']"))
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
  describe "actions" $ do
    it "finds the actions when reacting substrate and enzyme" $
      shouldBe
        (6.0 |> vect (simplify $ new [0] (enzymeSbound <|>  enzymeEbound))
          +> (-6.0) |> vect (Def "S" [] []) +> (-6.0) |> vect (Def "E" [] []))
        (actions affinityNetworkEnzyme (partialS +> partialE))
    it "can find the actions from a symbolic enzyme and substrate" $
      actions affinityNetworkEnzyme (partialS' +> partialE' :: D' Double)
        `shouldNotBe`
        vectZero
    it "can find the S component from symbolic enzyme and substrate" $
      simplify (vect (Def "S" [] []) <> actions affinityNetworkEnzyme (partialS' +> partialE'))
        `shouldBe`
        - (var "[E]" * var "[S]")
  describe "dPdt" $ do
    it "gives correct dPdt for reacting substrate and enzyme" $
      shouldBe
        (6.0 |> vect (simplify $ new [0] (enzymeSbound <|>  enzymeEbound))
          +> (-6.0) |> vect (Def "S" [] []) +> (-6.0) |> vect (Def "E" [] []))
        (dPdt enzymeDefs (React affinityNetworkEnzyme
          (Mixture [(3.0, Def "S" [] []), (2.0, Def "E" [] [])])))
    it "gives correct dPdt for enzyme substrate complex" $
      shouldBe
        (dPdt enzymeDefs (React affinityNetworkEnzyme (Mixture [(4.0, enzymeC)])))
        (12 |> vect (Def "P" [] []) +> 8 |> vect (Def "S" [] []) +> 20 |> vect (Def "E" [] []) +> (-20.0) |> vect (simplify enzymeC))
  describe "dPdt'" $ do
    it "gives nonzero dPdt for enzymes" $
      let tr = tracesGivenNetwork affinityNetworkEnzyme enzymeDefs
      in dPdt' tr affinityNetworkEnzyme (enzymeProc :: P' Double)
         `shouldNotBe` vectZero
