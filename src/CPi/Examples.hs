module CPi.Examples
  (
  simCH2,
  simRabbits,
  simLogisticRabbits,
  simEnzyme,
  simPolymer,
  rabbitDefs,
  affinityNetworkRabbits,
  defsCH2,
  affinityNetworkCH2,
  enzymeDefs,
  affinityNetworkEnzyme,
  affinityNetworkPolymer,
  polymerDefs,
  rabbitSource,
  rabbitModel,
  partialE,
  partialS,
  enzymeC,
  enzymeSbound,
  partialEnzymeMM,
  enzymeS'bound,
  enzymeS',
  enzymeDefs2,
  partialS',
  partialE',
  partialC,
  enzymeEbound,
  partialC',
  enzymeProc
  ) where

import CPi.AST
import CPi.Simulation
import CPi.Vector
import CPi.Base
import CPi.Processes (D,P',D')
import Prelude hiding ((*>))
import CPi.Symbolic
import qualified Data.Map as M

-- massAction :: RateLawFamily
-- massAction [k] xs = k * product xs
-- massAction _ _ = error "mass action called with wrong arguments"

defsCH2 :: Env
defsCH2 = M.fromList [
            ("CH2", SpeciesDef [] [] $ new [1] (Def "Left" [] [1] <|> Def "Right" [] [1])),
            ("Left", SpeciesDef [] [1] $ mkSum [(Unlocated "joinL", mkAbs 0 $ Def "LeftBound" [] [0,1])]),
            ("LeftBound", SpeciesDef [] [0,1] $ mkSum [(Located "unjoinL" 0, mkAbsBase $ Def "Left" [] [1])]),
            ("Right", SpeciesDef [] [1] $ mkSum [(Unlocated "joinR", mkAbs 0 $ Def "RightBound" [] [0,1])]),
            ("RightBound", SpeciesDef [] [0,1] $ mkSum [(Located "unjoinR" 0, mkAbsBase $ Def "Right" [] [1])])
          ]
affinityNetworkCH2 :: ConcreteAffinityNetwork
affinityNetworkCH2 = [ ConcreteAffinity (massAction [2]) [["joinL"], ["joinR"]], ConcreteAffinity (massAction [1]) [["unjoinL", "unjoinR"]] ]

simCH2 :: Trace
simCH2 = simulate defsCH2 affinityNetworkCH2 0.1 0 (1.0 |> vect (Def "CH2" [] []))

logistic :: RateLawFamily
logistic [b, k] = RateLaw $ \[r] -> fromFloat b * r * (1 - r/fromFloat k)
logistic _ = error "logistic called with wrong arguments"

functional :: RateLawFamily
functional [beta, h] = RateLaw $ \[f, r] -> fromFloat beta * f * r / (1 + fromFloat beta * fromFloat h * r)
functional _ = error "functional called with wrong arguments"

rabbitDefs :: Env
rabbitDefs = M.fromList
  [ ("Fox", SpeciesDef [] [] $ mkSum [ (Unlocated "eat",
                                      mkAbsBase $ Def "Fox" [] [] <|> Def "Fox" [] [])
                                   , (Unlocated "die", mkAbsBase Nil) ])
  , ("Red", SpeciesDef [] [] $ mkSum [ (Unlocated "reproduceRed", mkAbsBase $ Def "Red" [] [] <|> Def "Red" [] [])
                                   , (Unlocated "beEaten", mkAbsBase Nil)])
  , ("Blue", SpeciesDef [] [] $ mkSum [ (Unlocated "reproduceBlue", mkAbsBase $ Def "Blue" [] [] <|> Def "Blue" [] [])
                                   , (Unlocated "beEaten", mkAbsBase Nil)]) ]

affinityNetworkRabbits :: ConcreteAffinityNetwork
affinityNetworkRabbits =
  [ ConcreteAffinity (massAction [2]) [["reproduceRed"]]
  , ConcreteAffinity (massAction [3]) [["reproduceBlue"]]
  , ConcreteAffinity (functional [100, 2]) [["eat"], ["beEaten"]]
  , ConcreteAffinity (massAction [0.01]) [["die"]] ]

affinityNetworkLogisticRabbits :: ConcreteAffinityNetwork
affinityNetworkLogisticRabbits =
  [ ConcreteAffinity (logistic [2, 150]) [["reproduceRed"]]
  , ConcreteAffinity (logistic [3, 100]) [["reproduceBlue"]]
  , ConcreteAffinity (functional [10, 1]) [["eat"], ["beEaten"]]
  , ConcreteAffinity (massAction [0.01]) [["die"]] ]

simRabbits :: Trace
simRabbits = simulate rabbitDefs affinityNetworkRabbits 0.01 0 (1.0 |> vect (Def "Red" [] []) +> 1.0 |> vect (Def "Blue" [] []) +> 1.0 |> vect (Def "Fox" [] []))

simLogisticRabbits :: Trace
simLogisticRabbits = simulate rabbitDefs affinityNetworkLogisticRabbits 0.01 0 (1.0 |> vect (Def "Red" [] []) +> 1.0 |> vect (Def "Blue" [] []) +> 1.0 |> vect (Def "Fox" [] []))

affinityNetworkEnzyme :: ConcreteAffinityNetwork
affinityNetworkEnzyme =
  [ ConcreteAffinity (massAction [1]) [["e"], ["s"]]
  , ConcreteAffinity (massAction [2]) [["x", "r"]]
  , ConcreteAffinity (massAction [3]) [["x", "p"]] ]

enzymeSbound :: Species
enzymeSbound = mkSum [ (Located "r" 0, mkAbsBase (Def "S" [] []))
                     , (Located "p" 0, mkAbsBase (Def "P" [] [])) ]

enzymeS'bound :: Species
enzymeS'bound = mkSum [(Located "r" 0, mkAbsBase (Def "S'" [] [])),
                       (Located "p" 0, mkAbsBase (Def "P" [] []))]

enzymeS' :: SpeciesDefinition
enzymeS' = SpeciesDef [] [] $ mkSum [(Unlocated "s", mkAbs 0 enzymeS'bound)]

enzymeEbound :: Species
enzymeEbound = mkSum [(Located "x" 0, mkAbsBase (Def "E" [] []))]

enzymeE :: SpeciesDefinition
enzymeE = SpeciesDef [] [] $ mkSum [(Unlocated "e", mkAbs 0 enzymeEbound)]

enzymeS :: SpeciesDefinition
enzymeS = SpeciesDef [] [] $ mkSum [(Unlocated "s", mkAbs 0 enzymeSbound)]

enzymeP :: SpeciesDefinition
enzymeP = SpeciesDef [] [] $ mkSum [(Unlocated "d", mkAbsBase (Def "P" [] []))]

enzymeDefs :: Env
enzymeDefs = M.fromList [("E", enzymeE), ("S", enzymeS), ("P", enzymeP)]

enzymeDefs2 :: Env
enzymeDefs2 = M.fromList [("E", enzymeE), ("S", enzymeS), ("S'", enzymeS'), ("P", enzymeP)]

enzymeC :: Species
enzymeC = new [0] $ enzymeEbound <|> enzymeSbound

simEnzyme :: Trace
simEnzyme = simulate enzymeDefs affinityNetworkEnzyme 0.01 0 (3.0 |> vect (Def "S" [] []) +> 2.0 |> vect (Def "E" [] []))

polymerDefs :: Env
polymerDefs = M.fromList
  [ ("A", SpeciesDef [] [0] $ mkSum[(Unlocated "grow", mkAbsBase (Def "A" [] [0] <|> Def "A" [] [0])),
                                  (Unlocated "shrink", mkAbsBase Nil)])
  , ("B", SpeciesDef [] [] $ new [0] $ Def "A" [] [0]) ]

partialS :: D
partialS = 3.0 |> vect (Def "S" [] [])
           *> vect (simplify $ mkAbs 0 enzymeSbound) *> vect [Unlocated "s"]

partialE :: D
partialE = 2.0 |> vect (Def "E" [] [])
           *> vect (simplify $ mkAbs 0 enzymeEbound) *> vect [Unlocated "e"]

partialS' :: D'
partialS' = var "[S]" |> vect (Def "S" [] [])
           *> vect (simplify $ mkAbs 0 enzymeSbound) *> vect [Unlocated "s"]

partialE' :: D'
partialE' = var "[E]" |> vect (Def "E" [] [])
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

partialC' :: D'
partialC' = var "[C]" |> (vect (normalForm enzymeC
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
                  :* [Unlocated "p"]))

enzymeProc :: P'
enzymeProc = var "[S]" |> vect (Def "S" [] [])
          +> var "[E]" |> vect (Def "E" [] [])
          +> var "[C]" |> vect enzymeC
          +> var "[P]" |> vect (Def "P" [] [])

partialEnzymeMM :: D'
partialEnzymeMM
  =  var "[S]" |> vect (Def "S" [] [] :* mkAbsBase (Def "P" [] [])
                                      :* [Unlocated "s"])
  +> var "[P]" |> vect (Def "P" [] [] :* mkAbsBase (Def "P" [] [])
                                      :* [Unlocated "p"])
  +> var "[E]" |> vect (Def "E" [] [] :* mkAbsBase (Def "E" [] [])
                                      :* [Unlocated "e"])

affinityNetworkPolymer :: ConcreteAffinityNetwork
affinityNetworkPolymer =
  [ ConcreteAffinity (massAction [2]) [["grow"]]
  , ConcreteAffinity (massAction [1]) [["shrink"]] ]

simPolymer :: Trace
simPolymer = simulateUptoEpsilon 0.0001 polymerDefs affinityNetworkPolymer 0.01 0.01 0.001 0.0001 0.1 0 (1.0 |> vect (Def "B" [] []))


rabbitSource :: String
rabbitSource =
     "species Rabbit = reproduce -> (Rabbit|Rabbit)\n"
  ++ "               + beEaten;\n\n"
  ++ "species Fox = eat -> (Fox|Fox)\n"
  ++ "            + die;\n\n"
  ++ "affinity network MassActionRabbits = {\n"
  ++ "  reproduce at rate MA(1.0);\n"
  ++ "  eat, beEaten at rate MA(0.5);\n"
  ++ "  die at rate MA(0.05);\n"
  ++ "}\n\n"
  ++ "process FoxesAndRabbits = [10.0] Rabbit || [1.0] Fox\n"
  ++ "                          with network MassActionRabbits;"

rabbitModel :: CPiModel
rabbitModel = combineModels emptyCPiModel
  Defs { speciesDefs = M.fromList [
            ("Rabbit", SpeciesDef [] []
                       (mkSum [(Unlocated "reproduce", mkAbsBase $
                                mkPar [Def "Rabbit" [] [],
                                       Def "Rabbit" [] []]),
                               (Unlocated "beEaten", mkAbsBase Nil)])),
            ("Fox", SpeciesDef [] []
                    (mkSum [(Unlocated "eat", mkAbsBase $
                             mkPar [Def "Fox" [] [],
                                    Def "Fox" [] []]),
                            (Unlocated "die", mkAbsBase Nil)])) ]
        , affinityNetworkDefs = M.fromList [
            ("MassActionRabbits", AffinityNetworkDef []
                                  [ Affinity
                                    (RateLawAppl "MA"
                                                 [RateLawParamVal 1.0])
                                     [["reproduce"]]
                                  , Affinity
                                    (RateLawAppl "MA"
                                                 [RateLawParamVal 0.5])
                                    [["eat"], ["beEaten"]]
                                  , Affinity
                                    (RateLawAppl "MA"
                                                 [RateLawParamVal 0.05])
                                    [["die"]] ]) ]
        , processDefs = M.fromList [
            ("FoxesAndRabbits", Process (AffinityNetworkAppl
                                         "MassActionRabbits" [])
                                        [ (10.0, Def "Rabbit" [] [])
                                        , (1.0, Def "Fox" [] []) ]) ]
        , kineticLawDefs = M.empty }
