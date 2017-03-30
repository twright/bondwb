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
  polymerDefs
  ) where

import CPi.AST
import CPi.Processes
import CPi.Simulation
import CPi.Vector
import qualified Data.Map as M

massAction [k] xs = k * product xs

defsCH2 = M.fromList [
            ("CH2", SpeciesDef [] [] $ new [1] (Def "Left" [] [1] <|> Def "Right" [] [1])),
            ("Left", SpeciesDef [] [1] $ Sum [(Unlocated "joinL", Abs 0 $ Def "LeftBound" [] [0,1])]),
            ("LeftBound", SpeciesDef [] [0,1] $ Sum [(Located "unjoinL" 0, AbsBase $ Def "Left" [] [1])]),
            ("Right", SpeciesDef [] [1] $ Sum [(Unlocated "joinR", Abs 0 $ Def "RightBound" [] [0,1])]),
            ("RightBound", SpeciesDef [] [0,1] $ Sum [(Located "unjoinR" 0, AbsBase $ Def "Right" [] [1])])
          ]
affinityNetworkCH2 = [ Affinity (massAction [2]) [[Unlocated "joinL"], [Unlocated "joinR"]], Affinity (massAction [1]) [[Unlocated "unjoinL", Unlocated "unjoinR"]] ]

simCH2 = simulate defsCH2 affinityNetworkCH2 0.1 0 (1.0 |> vect (Def "CH2" [] []))

logistic [b, k] [r] = b * r * (1 - r/k)

functional [beta, h] [f, r] = beta * f * r / (1 + beta * h * r)

rabbitDefs = M.fromList
  [ ("Fox", SpeciesDef [] [] $ Sum [ (Unlocated "eat",
                                      AbsBase $ Def "Fox" [] [] <|> Def "Fox" [] [])
                                   , (Unlocated "die", AbsBase Nil) ])
  , ("Red", SpeciesDef [] [] $ Sum [ (Unlocated "reproduceRed", AbsBase $ Def "Red" [] [] <|> Def "Red" [] [])
                                   , (Unlocated "beEaten", AbsBase Nil)])
  , ("Blue", SpeciesDef [] [] $ Sum [ (Unlocated "reproduceBlue", AbsBase $ Def "Blue" [] [] <|> Def "Blue" [] [])
                                   , (Unlocated "beEaten", AbsBase Nil)]) ]

affinityNetworkRabbits =
  [ Affinity (massAction [2]) [[Unlocated "reproduceRed"]]
  , Affinity (massAction [3]) [[Unlocated "reproduceBlue"]]
  , Affinity (functional [100, 2]) [[Unlocated "eat"], [Unlocated "beEaten"]]
  , Affinity (massAction [0.01]) [[Unlocated "die"]] ]

affinityNetworkLogisticRabbits =
  [ Affinity (logistic [2, 150]) [[Unlocated "reproduceRed"]]
  , Affinity (logistic [3, 100]) [[Unlocated "reproduceBlue"]]
  , Affinity (functional [10, 1]) [[Unlocated "eat"], [Unlocated "beEaten"]]
  , Affinity (massAction [0.01]) [[Unlocated "die"]] ]

simRabbits = simulate rabbitDefs affinityNetworkRabbits 0.01 0 (1.0 |> vect (Def "Red" [] []) +> 1.0 |> vect (Def "Blue" [] []) +> 1.0 |> vect (Def "Fox" [] []))

simLogisticRabbits = simulate rabbitDefs affinityNetworkLogisticRabbits 0.01 0 (1.0 |> vect (Def "Red" [] []) +> 1.0 |> vect (Def "Blue" [] []) +> 1.0 |> vect (Def "Fox" [] []))

affinityNetworkEnzyme =
  [ Affinity (massAction [1]) [[Unlocated "e"], [Unlocated "s"]]
  , Affinity (massAction [2]) [[Unlocated "x", Unlocated "r"]]
  , Affinity (massAction [3]) [[Unlocated "x", Unlocated "p"]] ]

enzymeSbound = Sum [ (Located "r" 0, AbsBase (Def "S" [] []))
                   , (Located "p" 0, AbsBase (Def "P" [] [])) ]

enzymeEbound = Sum [(Located "x" 0, AbsBase (Def "E" [] []))]

enzymeE = SpeciesDef [] [] $ Sum [(Unlocated "e", Abs 0 enzymeEbound)]

enzymeS = SpeciesDef [] [] $ Sum [(Unlocated "s", Abs 0 enzymeSbound)]

enzymeP = SpeciesDef [] [] $ Sum [(Unlocated "d", AbsBase (Def "P" [] []))]

enzymeDefs = M.fromList [("E", enzymeE), ("S", enzymeS), ("P", enzymeP)]

simEnzyme = simulate enzymeDefs affinityNetworkEnzyme 0.01 0 (3.0 |> vect (Def "S" [] []) +> 2.0 |> vect (Def "E" [] []))

polymerDefs = M.fromList
  [ ("A", SpeciesDef [] [0] $ Sum[(Unlocated "grow", AbsBase (Def "A" [] [0] <|> Def "A" [] [0])),
                                  (Unlocated "shrink", AbsBase Nil)])
  , ("B", SpeciesDef [] [] $ new [0] $ Def "A" [] [0]) ]

affinityNetworkPolymer =
  [ Affinity (massAction [2]) [[Unlocated "grow"]]
  , Affinity (massAction [1]) [[Unlocated "shrink"]] ]

simPolymer = simulateUptoEpsilon 0.0001 polymerDefs affinityNetworkPolymer 0.01 0 (1.0 |> vect (Def "B" [] []))
