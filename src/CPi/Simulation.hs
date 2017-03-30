{-# LANGUAGE BangPatterns #-}

module CPi.Simulation
  (
  Trace,
  toProcess,
  simulate,
  formatTrace,
  simulateUptoEpsilon,
  uptoEpsilon
  ) where

import CPi.AST
import CPi.Processes
import CPi.Transitions
import CPi.Vector
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.HashMap.Strict as H

type Trace = [(Double, P)]

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

toProcess :: P -> Process
toProcess (Vect v) = Mixture [(abs x, spec) | (spec, x) <- H.toList v]

simulate :: Env -> AffinityNetwork -> Double -> Double -> P -> Trace
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
        uptoEpsilon' epsilon l@((_, s):svs)
          | abs s < epsilon = uptoEpsilon' (epsilon - abs s)
                                                        svs
          | otherwise = l
        key (_, s) = abs s
        -- makeKet :: [(Scalar, P)] -> P
        -- makeKet ((s, v):xs) = s |> v +> makeKet xs
        -- makeKet [] = KetZero

-- simulateUptoEpsilon :: Double -> Env -> AffinityNetwork -> Double -> Double -> P -> Trace
-- simulateUptoEpsilon epsilon env network h !t !p0 = (t, p0) : simulateUptoEpsilon epsilon env network h t' p'
--   where t'   = t + h
--         dpdt = dPdt' tr network p0
--         p'   = uptoEpsilon epsilon $ p0 +> (h :+ 0) |> dpdt
--         tr   = transFiltered validPref env
--         prefLists = L.nub $ L.sort $ map (map prefName) $ L.concatMap affSites network
--         prefListSubsets = L.nub $ L.sort $ L.concatMap powerset prefLists
--         validPref :: PrefixFilter
--         validPref Potential = prefListSubsets `seq` (`elem` prefListSubsets)
--         validPref Final = prefLists `seq` (`elem` prefLists)

sim tr network epsilon h !t !p0 = (t, p0) : sim tr network epsilon h t' p'
  where t'   = t + h
        dpdt = dPdt' tr network p0
        p'   = uptoEpsilon epsilon $ p0 +> h |> dpdt

simulateUptoEpsilon :: Double -> Env -> AffinityNetwork -> Double -> Double -> P -> Trace
simulateUptoEpsilon epsilon env network = tr `seq` sim tr network epsilon
  where tr = validPref `seq` transFiltered validPref env
        prefLists = L.nub $ L.sort $ map (map prefName) $ L.concatMap affSites network
        prefListSubsets = L.nub $ L.sort $ L.concatMap powerset prefLists
        validPref :: PrefixFilter
        validPref Potential = prefListSubsets `seq` (`elem` prefListSubsets)
        validPref Final = prefLists `seq` (`elem` prefLists)

formatTrace :: Trace -> [(Double, [(String, Conc)])]
formatTrace tr = [(t, [(pretty v, conc) | (v, conc)
                        <- H.toList p]) | (t, Vect p) <- tr]
