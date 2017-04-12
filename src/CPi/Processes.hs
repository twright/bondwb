{-# LANGUAGE  FlexibleInstances, BangPatterns, MultiParamTypeClasses, FlexibleContexts #-}

module CPi.Processes (Process(..), Affinity(..), ConcreteAffinityNetwork,
  P, D, concretifyModel, partial, conc, direct, react, hide,
  networkSites, actions, dPdt, partial', dPdt', tracesGivenNetwork) where

import Prelude hiding ((*>), (<*))
import CPi.AST
import CPi.Transitions
import qualified Data.List as L
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import CPi.Vector
import Data.Maybe
import Data.Bifunctor
import Data.Hashable
-- import Debug.Trace
trace _ b = b

instance Nullable Conc where
  isnull = (<0.001)

-- Process space
type P = Vect Species Conc
-- Potential interaction space
type D = Vect (Tensor (Tensor Species Abstraction) [Prefix]) Conc

data Process = Mixture [(Conc, Species)]
             | React ConcreteAffinityNetwork Process

-- A CPi model with all of the variables looked up
type ConcreteModel = (Env, M.Map String (ConcreteAffinityNetwork, P))

concretifyAffSpec :: CPiModel -> AffinityNetworkSpec
                              -> Either String ConcreteAffinityNetwork
concretifyAffSpec model@(Defs _ a _ _) (AffinityNetworkAppl name rates)
  = case M.lookup name a of
      Just (AffinityNetworkDef params body) ->
        if length rates == length rates
        then networkFromList [applyAff model aff params rates | aff <- body]
        else Left $ "Affinity network " ++ name ++ " applied to the "
                     ++ "wrong number of arguments."
      Nothing -> Left $ "Affinity network " ++ name ++ " does not exist."
    where networkFromList :: [Either String ConcreteAffinity]
                             -> Either String ConcreteAffinityNetwork
          networkFromList affsOrErrors = case errors of
                                           []    -> Right affs
                                           (e:_) -> Left e
            where errors = [e | Left e <- affsOrErrors]
                  affs   = [aff | Right aff <- affsOrErrors]

applyAff :: CPiModel -> Affinity -> [String] -> [Rate] -> Either String ConcreteAffinity
applyAff model (Affinity rls sites) params rates
  = second (`ConcreteAffinity` sites) $ applyRateLawSpec model rls params rates

applyRateLawSpec :: CPiModel -> RateLawSpec -> [String] -> [Rate]
                             -> Either String RateLaw
applyRateLawSpec (Defs _ _ k _) (RateLawAppl name params) affParams rates
  = case M.lookup name k of
      Nothing -> Left $ "Kinetic law " ++ name ++ " does not exist."
      Just rlf -> if any isNothing args
                  then Left $ "Kinetic law " ++ name ++ " applied to "
                               ++ "variables which do not exist."
                  else Right $ rlf $ map (fromMaybe undefined) args
    where vals = zip affParams rates
          applyRate (RateLawParamVal x) = Just x
          applyRate (RateLawParamVar n) = L.lookup n vals
          args = map applyRate params

concretifyProcess :: CPiModel -> AbstractProcess
                     -> Either String (ConcreteAffinityNetwork, P)
concretifyProcess model (Process affSpec concSpecs)
  = case networkOrError of
      Right network -> Right (network, p)
      Left err    -> Left err
  where p              = fromList [(c, normalForm s) | (c,s) <- concSpecs]
        networkOrError = concretifyAffSpec model affSpec

concretifyModel :: CPiModel -> Either String ConcreteModel
concretifyModel model@(Defs env _ _ p)
  = case errors of
      []            -> Right $ (env, processes)
      ((name, e):_) -> Left $ "Error found in process definition " ++ name
                               ++ ":\n" ++ e
  where processes = M.fromList [ (name,  process)
                    | (name, Right process) <- cprocs]
        cprocs    = M.toList $ fmap (concretifyProcess model) p
        errors    = [(name, e) | (name, Left e) <- cprocs]

conc :: [Prefix] -> D -> Conc
conc s (Vect v) = norm $ Vect $ H.filterWithKey atS v
  where atS (_ :* _ :* t) _ = s == t

direct :: [Prefix] -> D -> Vect (Tensor Species Abstraction) Conc
direct l v = normalize (direct' >< v)
  where direct' (s :* s' :* m) = fromInteger (delta l m) |> vect (s :* s')

networkSites :: ConcreteAffinityNetwork -> [[Prefix]]
networkSites = L.concatMap (map (map Unlocated) . cAffSites)

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

tracesGivenNetwork :: ConcreteAffinityNetwork -> Env -> Species -> MTS
tracesGivenNetwork network = trace ("prefLists = " ++ show prefLists) (transFiltered validPref)
  where
    prefLists = L.nub $ L.sort $ map (L.sort . map prefName) $ L.concatMap (map (map Unlocated) . cAffSites) network
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

actions :: ConcreteAffinityNetwork -> D -> P
actions network !potential = L.foldl' (+>) vectZero [
  let concs   = map (`conc` potential) sites'
      directs = map (`direct` potential) sites'
      sites'  = map (L.sort . map Unlocated) sites
  in concs `seq` directs `seq` law concs |> react directs
  | ConcreteAffinity law sites <- network ]

partial' :: (Species -> MTS) -> P -> D
partial' tr = (partial'' ><)
  where partial'' spec = fromList [(1, s :* s' :* a)
                           | Trans _ s a s' <- simplify $ tr spec]

dPdt' :: (Species -> MTS) -> ConcreteAffinityNetwork -> P -> P
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
