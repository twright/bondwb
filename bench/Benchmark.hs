import Criterion.Main

import CPi.AST
import QuantumVector
import CPi.Simulation
import CPi.Examples

benchUptoEpsilon :: Int -> Double -> [(Double, [(String, Conc)])]
benchUptoEpsilon n epsilon = take n $ formatTrace
  $ simulateUptoEpsilon epsilon polymerDefs
    affinityNetworkPolymer 0.01 0
    (1.0 |> Ket (Def "B" [] []))

main :: IO ()
main = defaultMain
      [ bench "simulate polymers, 10 steps, epsilon = 0.001"
          $ nf (benchUptoEpsilon 10) 0.001
      , bench "simulate polymers, 10 steps, epsilon = 0.0001"
          $ nf (benchUptoEpsilon 10) 0.0001
      , bench "simulate polymers, 100 steps, epsilon = 0.001"
          $ nf (benchUptoEpsilon 100) 0.001
      , bench "simulate polymers, 50 steps, epsilon = 0.0001"
          $ nf (benchUptoEpsilon 50) 0.0001 ]
