import Criterion.Main

import CPi.AST
import CPi.Vector
import CPi.Simulation
import CPi.Examples

benchUptoEpsilon :: Int -> Double -> [(Double, [(String, Conc)])]
benchUptoEpsilon n epsilon = take n $ formatTrace
  $ simulateUptoEpsilon epsilon polymerDefs
    affinityNetworkPolymer 0.01 0
    (1.0 |> vect (Def "B" [] []))

benchNormalForm :: Int -> String
benchNormalForm n = pretty $ normalForm (spec (fromIntegral n) Nil)
  where spec n inner | n <= 0 = inner
                     | otherwise = mkNew [n] (spec (n - 1)
                                           (inner <|> Def "E" [] [n]))

benchUptoEpsilonEnzyme :: Int -> Double -> [(Double, [(String, Conc)])]
benchUptoEpsilonEnzyme n epsilon = take n $ formatTrace
  $ simulateUptoEpsilon epsilon enzymeDefs
    affinityNetworkEnzyme 0.01 0
    (1.0 |> vect (Def "E" [] []) +> 1.0 |> vect (Def "S" [] []))

main :: IO ()
main = defaultMain
      [ bench "simulate polymers, 10 steps, epsilon = 0.001"
          $ nf (benchUptoEpsilon 10) 0.001
      , bench "simulate polymers, 10 steps, epsilon = 0.0001"
          $ nf (benchUptoEpsilon 10) 0.0001
      , bench "simulate polymers, 100 steps, epsilon = 0.001"
          $ nf (benchUptoEpsilon 100) 0.001
      , bench "simulate polymers, 50 steps, epsilon = 0.0001"
          $ nf (benchUptoEpsilon 50) 0.0001
      , bench "simulate enzymes, 10000 steps, epsilon = 0.00001"
          $ nf (benchUptoEpsilonEnzyme 10000) 0.00001
      , bench "simulate enzymes, 5000 steps, epsilon = 0.00001"
          $ nf (benchUptoEpsilonEnzyme 5000) 0.00001
      , bench "simulate creating normal form, 100 deep"
          $ nf benchNormalForm 100
      , bench "simulate creating normal form, 200 deep"
          $ nf benchNormalForm 200 ]
