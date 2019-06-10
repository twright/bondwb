module BondCalculus.StochPyExtractionSpec (spec) where

import Prelude hiding ((*>))

import Test.Hspec
-- import Data.Map (Map)
import qualified Data.Map as M
import BondCalculus.StochPyExtraction
import BondCalculus.Symbolic hiding (Abs)
import BondCalculus.Vector
import BondCalculus.Examples (rabbitModel)
import BondCalculus.AST
import BondCalculus.Processes (concretifyAffSpec, tracesGivenNetwork)
import Debug.Trace
import Data.Either

spec :: SpecWith ()
spec = do
  describe "reaction" $ do
    it "can react boring processes" $
      (reaction [vect(Def "A" [] [] :* mkAbsBase (Def "B" [] [])), vect(Def "C" [] [] :* mkAbsBase (Def "D" [] []))] :: ReactionVect Conc)
        `shouldBe`
        vect((vect(Def "A" [] []) +> vect(Def "C" [] []))
          :* (vect(Def "B" [] []) +> vect(Def "D" [] [])))
    it "can react boring processes upto pretty printing" $
      pretty (reaction [vect(Def "A" [] [] :* mkAbsBase (Def "B" [] [])), vect(Def "C" [] [] :* mkAbsBase (Def "D" [] []))] :: ReactionVect Conc)
        `shouldBe`
        pretty (vect((vect(Def "A" [] []) +> vect(Def "C" [] []))
          :* (vect(Def "B" [] []) +> vect(Def "D" [] []))) :: ReactionVect Conc)
  describe "reactions" $ do
    it "finds correct reaction vector for rabbitModel" $
      let Defs env _ _ _ = rabbitModel
          Right network = concretifyAffSpec
                          rabbitModel
                          (AffinityNetworkAppl "MassActionRabbits" [])
          p = var "Rabbit" |> vect (Def "Rabbit" [] []) +>
              var "Fox" |> vect (Def "Fox" [] [])
          tr = tracesGivenNetwork network env
      in reactions tr network p
          `shouldBe` vectZero
  describe "extractReactionSystem" $ do
    it "extracts reaction system for rabbit model" $
      let Defs env _ _ _ = rabbitModel
          Right network = concretifyAffSpec
                          rabbitModel
                          (AffinityNetworkAppl "MassActionRabbits" [])
          p = var "Rabbit" |> vect (Def "Rabbit" [] []) +>
              var "Fox" |> vect (Def "Fox" [] [])
      in extractReactionSystem env network p [10.0, 1.0]
          `shouldBe` ReactionSystem ([], [], [])
