{-# LANGUAGE BangPatterns #-}

module CPi.Simulation
  (
  Trace,
  toProcess,
  simulate,
  formatTrace,
  simulateUptoEpsilon,
  uptoEpsilon,
  simulateMaxSpecies,
  maxSpecies
  ) where

import CPi.AST
import CPi.Processes
import CPi.Vector
import CPi.NumericalMethods (gear, rungeKutta4, nordseik, startNordseik, adaptiveAM3)
import qualified Data.List as L
import qualified Data.HashMap.Strict as H

type Trace = [(Double, P)]

toProcess :: P -> Process
toProcess (Vect v) = Mixture [(abs x, spec) | (spec, x) <- H.toList v]

simulate :: Env -> ConcreteAffinityNetwork -> Double -> Double -> P -> Trace
simulate env network h !t !p0 = (t, p0) : simulate env network h t' p'
  where p    = toProcess p0
        t'   = t + h
        dpdt = dPdt env (React network p)
        p'   = p0 +> h |> dpdt

uptoEpsilon :: Conc -> P -> P
uptoEpsilon epsilon (Vect v) = fromList $ map (\(a, b) -> (b, a))
                      $ uptoEpsilon' epsilon
                      $ L.sortOn key $ H.toList v
  where uptoEpsilon' :: Conc -> [(Species, Conc)] -> [(Species, Conc)]
        uptoEpsilon' ep l@((_, s):svs)
          | abs s < ep = uptoEpsilon' (ep - abs s)
                                                        svs
          | otherwise = l
        uptoEpsilon' _  [] = []
        key (_, s) = abs s

simulateMaxSpecies :: Int -> Env -> ConcreteAffinityNetwork -> Double -> Double -> Double -> Double -> P -> Trace
simulateMaxSpecies n env network tol h hmin t0 p0
  =
  adaptiveAM3 f (maxSpecies n) tol hmin h t0 p0 p1 p2
  -- simAM3Modified2 f (maxSpecies n) tol 100 hmin h t0 p0 p1 p2
  -- gear f (maxSpecies n) tol 0 True 10 hmin h t0 p0 p1 p2 p3 p4
  -- nordseik f (maxSpecies n) tol 0 1000 hmin h h t0 p0 (f p0) vectZero vectZero vectZero vectZero
  -- startNordseik f (maxSpecies n) tol 0 1000 10 hmin h h t0 p0 (f p0) vectZero vectZero vectZero vectZero
  -- simRK4 f (maxSpecies n) h t0 p0
  where f  = dPdt' tr network
        _:(_, p1):(_, p2):(_,p3):(_,p4):_ = rungeKutta4 f (maxSpecies n) h t0 p0
        tr = tracesGivenNetwork network env

simulateUptoEpsilon :: Double -> Env -> ConcreteAffinityNetwork -> Double -> Double -> Double -> Double -> P -> Trace
simulateUptoEpsilon epsilon env network tol h hmin t0 p0
  = --simA3Adaptive f (uptoEpsilon epsilon) 1e-6 (h/100) h t0 p0 p1 p2
  adaptiveAM3 f (uptoEpsilon epsilon) tol hmin h t0 p0 p1 p2
  --  gear f (uptoEpsilon epsilon) tol 0 True 10 hmin h t0 p0 p1 p2 p3 p4
  -- simAM3Modified f (uptoEpsilon epsilon) (5*epsilon) 10000 h t0 p0 p1 p2
  where f  = dPdt' tr network
        _:(_, p1):(_, p2):(_,p3):(_,p4):_ = rungeKutta4 f (uptoEpsilon epsilon) h t0 p0
        tr = tracesGivenNetwork network env

maxSpecies :: Int -> P -> P
maxSpecies n (Vect v) = fromList $ map (\(a, b) -> (b, a))
                      $ take n
                      $ L.sortOn key $ H.toList v
  where key (_, s) = - abs s

formatTrace :: Trace -> [(Double, [(String, Conc)])]
formatTrace tr = [(t, [(pretty v, c) | (v, c)
                        <- H.toList p]) | (t, Vect p) <- tr]
