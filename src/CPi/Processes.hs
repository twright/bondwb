{-# LANGUAGE  FlexibleInstances, BangPatterns #-}

module CPi.Processes (Process(..), Affinity(..), AffinityNetwork,
  P, D, partial, conc, direct, react, hide,
  networkSites, actions, dPdt, partial', dPdt', tracesGivenNetwork) where

import Prelude hiding ((*>), (<*))
import CPi.AST
import CPi.Transitions
import qualified Data.List as L
import qualified Data.HashMap.Strict as H
import CPi.Vector
-- import Debug.Trace
trace a b = b

-- Process space
type P = Vect Species Conc
-- Potential interaction space
type D = Vect (Tensor (Tensor Species Abstraction) [Prefix]) Conc

data Affinity = Affinity { affRateLaw :: RateLaw
                         , affSites   :: [[Prefix]] }

type AffinityNetwork = [Affinity]

data Process = Mixture [(Conc, Species)]
             | React AffinityNetwork Process

conc :: [Prefix] -> D -> Conc
conc s (Vect v) = norm $ Vect $ H.filterWithKey atS v
  where atS (_ :* _ :* t) _ = s == t

direct :: [Prefix] -> D -> Vect (Tensor Species Abstraction) Conc
direct l v = normalize (direct' >< v)
  where direct' (s :* s' :* m) = fromInteger (delta l m) |> vect (s :* s')

networkSites :: AffinityNetwork -> [[Prefix]]
networkSites = L.concatMap affSites

hide :: [[Prefix]] -> D -> D
hide toHide (Vect v) = Vect $ H.filterWithKey shouldHide v
  where shouldHide (_ :* _ :* sites) _ = sites `notElem` toHide

partial :: Env -> Process -> D
partial env (Mixture ((c,spec):xs)) = fromList
                                      [(c, s :* s' :* a)
                                      | Trans _ s a s' <- simplify $ trans spec env]
                                      +> partial env (Mixture xs)
partial env (React network p) = hide (networkSites network) $ partial env p
partial _ (Mixture []) = vectZero

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

tracesGivenNetwork :: AffinityNetwork -> Env -> Species -> MTS
tracesGivenNetwork network = trace ("prefLists = " ++ show prefLists) (transFiltered validPref)
  where
    prefLists = L.nub $ L.sort $ map (L.sort . map prefName) $ L.concatMap affSites network
    prefListSubsets = L.nub $ L.sort $ L.concatMap powerset prefLists
    validPref :: PrefixFilter
    validPref Potential x = trace ("testing potential " ++ show x) (L.sort x `elem` prefListSubsets)
    validPref Final x = trace ("testing final " ++ show x) (L.sort x `elem` prefLists)

primes :: Species -> [Species]
primes (Par _ _ ss) = L.concatMap primes ss
primes s = [s]

embed :: Species -> P
embed spec = fromList $ map (\s -> (1, s)) $ filter (/=Nil) $ primes $ normalForm spec

react :: [Vect (Tensor Species Abstraction) Conc] -> P
react = multilinear react'
  where react' xs = embed (concretify $
                    foldl (<|>) (mkAbsBase Nil) (map target xs)) +>
                    (-1.0) |> foldl (+>) vectZero (map (embed.source) xs)
        source (spec :* _) = spec
        target (_ :* spec') = spec'

actions :: AffinityNetwork -> D -> P
actions network !potential = L.foldl' (+>) vectZero [
  let concs   = map (`conc` potential) sites'
      directs = map (`direct` potential) sites'
      sites'  = map L.sort sites
  in concs `seq` directs `seq` law concs |> react directs
  | Affinity law sites <- network ]

partial' :: (Species -> MTS) -> P -> D
partial' tr = (partial'' ><)
  where partial'' spec = fromList [(1, s :* s' :* a)
                           | Trans _ s a s' <- simplify $ tr spec]

dPdt' :: (Species -> MTS) -> AffinityNetwork -> P -> P
dPdt' tr network p = trace ("part = " ++ show part ++ ", acts = " ++ show acts)
                           acts
  where acts = actions network part
        part = partial' tr p

embedProcess :: Process -> P
embedProcess (Mixture ps) = fromList ps

dPdt :: Env -> Process -> P
dPdt _ (Mixture _) = vectZero
dPdt env (React network p) = dPdt' tr network pro
  where pro = embedProcess p
        tr  = tracesGivenNetwork network env
-- TODO: nested Mixtures inside Reacts
