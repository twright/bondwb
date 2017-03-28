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
import QuantumVector
import Data.Complex
import qualified Data.Map as M
import qualified Data.List as L

type Trace = [(Double, P)]

toProcess :: P -> Process
toProcess p = Mixture [(magnitude x, spec) | (x, Ket spec) <- zip (components p) (basis p)]

simulate :: Env -> AffinityNetwork -> Double -> Double -> P -> Trace
simulate env network h !t !p0 = (t, p0) : simulate env network h t' p'
  where p    = toProcess p0
        t'   = t + h
        dpdt = dPdt env (React network p)
        p'   = p0 +> (h :+ 0) |> dpdt

uptoEpsilon :: Double -> P -> P
uptoEpsilon epsilon v = makeKet
                      $ uptoEpsilon' epsilon
                      $ L.sortOn key $ zip (components v) (basis v)
  where uptoEpsilon' :: Double -> [(Scalar, P)] -> [(Scalar, P)]
        uptoEpsilon' epsilon l@((s, Ket v):svs)
          | magnitude s < epsilon = uptoEpsilon' (epsilon - magnitude s)
                                                        svs
          | otherwise = l
        key (s, _) = magnitude s
        makeKet :: [(Scalar, P)] -> P
        makeKet ((s, v):xs) = s |> v +> makeKet xs
        makeKet [] = KetZero

simulateUptoEpsilon :: Double -> Env -> AffinityNetwork -> Double -> Double -> P -> Trace
simulateUptoEpsilon epsilon env network h !t !p0 = (t, p0) : simulateUptoEpsilon epsilon env network h t' p'
  where p    = toProcess p0
        t'   = t + h
        dpdt = dPdt env (React network p)
        p'   = uptoEpsilon epsilon $ p0 +> (h :+ 0) |> dpdt

formatTrace :: Trace -> [(Double, [(String, Conc)])]
formatTrace tr = [(t, [(pretty v, magnitude conc) | (Ket v, conc)
                        <- zip (basis p) (components p)]) | (t, p) <- tr]
