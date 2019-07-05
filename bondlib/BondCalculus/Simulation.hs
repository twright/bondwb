{-# LANGUAGE BangPatterns #-}

module BondCalculus.Simulation
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

import BondCalculus.AST
import BondCalculus.Processes
import BondCalculus.Vector
import BondCalculus.Transitions
import BondCalculus.NumericalMethods (adaptiveAM3, rungeKutta4)
import qualified Data.List as L
import qualified Data.HashMap.Strict as H

type Trace = [(Double, P)]

toProcess :: P -> Process
toProcess (Vect v) = Mixture [(abs x, spec) | (spec, x) <- H.toList v]

simulate :: Env -> ConcreteAffinityNetwork Conc -> Double -> Double -> P -> Trace
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
          | s < ep = uptoEpsilon' (ep - min s 0) svs
          | otherwise = l
        uptoEpsilon' _  [] = []
        key (_, s) = s

simulateMaxSpecies :: Int -> Env -> ConcreteAffinityNetwork Conc -> Double -> Double -> Double -> Double -> Double -> Double ->  P -> Trace
simulateMaxSpecies n env network tolabs tolrel h hmin hmax t0 p0
  =
  adaptiveAM3 f (maxSpecies n) tolabs tolrel hmin hmax h t0 p0 p1 p2
  -- simAM3Modified2 f (maxSpecies n) tol 100 hmin h t0 p0 p1 p2
  -- gear f (maxSpecies n) tolabs tolrel True 10 hmin hmax h t0 p0 p1 p2 p3 p4
  -- nordseik f (maxSpecies n) tol 0 1000 hmin h h t0 p0 (f p0) vectZero vectZero vectZero vectZero
  -- rungeKuttaFehlberg45 f (maxSpecies n) h hmin hmax tolabs tolrel t0 p0
  -- startNordseik f (maxSpecies n) tol 0 1000 10 hmin h h t0 p0 (f p0) vectZero vectZero vectZero vectZero
  -- rungeKutta4 f (maxSpecies n) h t0 p0
  where f  = dPdt''' tr network
        _:(_, p1):(_, p2):(_,_p3):(_,_p4):_ = rungeKutta4 f (maxSpecies n) h t0 p0
        tr = trans env
          -- tracesGivenNetwork network env

simulateUptoEpsilon :: Double -> Env -> ConcreteAffinityNetwork Conc -> Double -> Double -> Double -> Double -> Double -> Double -> P -> Trace
simulateUptoEpsilon epsilon env network tolabs tolrel h hmin hmax t0 p0
  = --simA3Adaptive f (uptoEpsilon epsilon) 1e-6 (h/100) h t0 p0 p1 p2
  --  adaptiveAM3 f (uptoEpsilon epsilon) tol hmin h t0 p0 p1 p2
  adaptiveAM3 f (uptoEpsilon epsilon) tolabs tolrel hmin hmax h t0 p0 p1 p2
  --  gear f (uptoEpsilon epsilon) tolabs tolrel True 10 hmin hmax h t0 p0 p1 p2 p3 p4
  -- rungeKuttaFehlberg45 f (uptoEpsilon epsilon) h hmin hmax tolabs tolrel t0 p0
  -- simAM3Modified f (uptoEpsilon epsilon) (5*epsilon) 10000 h t0 p0 p1 p2
  -- rungeKutta4 f (uptoEpsilon epsilon) h t0 p0
  where f  = dPdt''' tr network
        _:(_, p1):(_, p2):(_,_p3):(_,_p4):_ = rungeKutta4 f (uptoEpsilon epsilon) h t0 p0
        -- tr = trans env
        tr = tracesGivenNetwork network env

maxSpecies :: Int -> P -> P
maxSpecies n (Vect v) = fromList $ map (\(a, b) -> (b, a))
                      $ take n
                      $ filter (\(_,s) -> s>0)
                      $ L.sortOn key $ H.toList v
  where key (_, s) = - abs s

formatTrace :: Trace -> [(Double, [(String, Conc)])]
formatTrace tr = [(t, [(pretty v, c) | (v, c)
                        <- H.toList p]) | (t, Vect p) <- tr]
