{-# LANGUAGE  FlexibleInstances, BangPatterns, MultiParamTypeClasses, FlexibleContexts, MonoLocalBinds #-}

module BondCalculus.Processes (Process(..), Affinity(..), ProcessVect,
  InteractionVect, DirectionVect, ConcreteAffinityNetwork, SymbolicModel,
  SymbolicDef, ConcreteModel, ConcreteDef, P, D, P', D',
  concretifyModel, partial, conc, direct, react, hide, networkSites,
  actions, dPdt, partial', dPdt', dPdt'', tracesGivenNetwork, speciesVar,
  concretifyAffSpec, symbolifyModel, embed, concretify, actions',
  symbolifyProcess, concretifyProcess) where

import Prelude hiding ((*>), (<*))
import BondCalculus.AST
import BondCalculus.Base
import BondCalculus.Transitions
import qualified Data.List as L
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import BondCalculus.Vector hiding ((<>))
import BondCalculus.Symbolic (SymbolicExpr, var)
import Data.Maybe
import Data.Either
import Data.Either.Utils
import Data.Bifunctor
import Control.Monad
-- import Debug.Trace
import GHC.Exts (sortWith)
trace :: String -> a -> a
trace _ = id

type SymbConc = SymbolicExpr

-- Process vectors over k
type ProcessVect k = Vect Species k
-- Interaction vectors over k
type InteractionVect k = Vect (Tensor (Tensor Species Abstraction) [Prefix]) k
-- Direction vectors over k
type DirectionVect k = Vect (Tensor Species Abstraction) k

-- Process space
type P = ProcessVect Conc
-- Potential interaction space
type D = InteractionVect Conc

-- Process space
type P' = ProcessVect SymbConc
-- Potential interaction space
type D' = InteractionVect SymbConc

data Process = Mixture [(Conc, Species)]
             | React ConcreteAffinityNetwork Process

-- A BondCalculus model with all of the variables looked up
type ConcreteDef = (ConcreteAffinityNetwork, Species -> MTS, P)
type ConcreteModel = (Env, M.Map String ConcreteDef)
type SymbolicDef = (ConcreteAffinityNetwork, Species -> MTS, P', [Double])
type SymbolicModel = (Env, M.Map String SymbolicDef)

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

-- Core process semantics

conc :: (Vector k (InteractionVect k)) => [Prefix] -> InteractionVect k -> k
conc s (Vect v) = norm $ Vect $ H.filterWithKey atS v
  where atS (_ :* _ :* t) _ = s == t

direct :: (Vector k (InteractionVect k), Vector k (DirectionVect k)) => [Prefix] -> InteractionVect k -> Vect (Tensor Species Abstraction) k
direct l v = normalize (direct' >< v)
  where direct' (s :* s' :* m) = fromInteger (delta l m) |> vect (s :* s')

hide :: (Vector k (InteractionVect k)) => [[Prefix]] -> InteractionVect k -> InteractionVect k
hide toHide (Vect v) = Vect $ H.filterWithKey shouldHide v
  where shouldHide (_ :* _ :* sites) _ = sites `notElem` toHide

primes :: Species -> [Species]
primes (Par _ _ ss) = L.concatMap primes ss
primes s = [s]

embed :: (Vector k (ProcessVect k)) => Species -> ProcessVect k
embed spec = fromList $ map (\s -> (1, s)) $ filter (/=Nil) $ primes $ normalForm spec

react :: (Vector k (DirectionVect k), Nullable (DirectionVect k), Expression k) =>
         [DirectionVect k] -> ProcessVect k
-- it is very important that reactions ignore zero vectors, otherwise we get
-- 0 (when we should just get the reaction of those actions which are present)
-- how does this work in this the symbolic case?
react = multilinear react' <$> filter (/=vectZero)
  where react' xs = embed (concretify $
                    foldl (<|>) (mkAbsBase Nil) (map target xs)) +>
                    (-1.0) |> foldl (+>) vectZero (map (embed.source) xs)
        source (spec :* _) = spec
        target (_ :* spec') = spec'

actions :: (Vector k (DirectionVect k), Nullable (DirectionVect k), DoubleExpression k, Show k) =>
           ConcreteAffinityNetwork -> InteractionVect k -> ProcessVect k
actions = actions' react

-- actions, but without specifying how to compute reactions
actions' :: (Vector k (DirectionVect k), Nullable (DirectionVect k), DoubleExpression k, Vector k v, Show k, Pretty v) =>
           ([DirectionVect k] -> v) -> ConcreteAffinityNetwork -> InteractionVect k -> v
actions' react' network !potential = L.foldl' (+>) vectZero [
  let concs   = map (`conc` potential') sites'
      directs = map (`direct` potential') sites'
      sites'  = map (L.sort . map Unlocated) sites
      magnitude = law concs
      effect  = react' directs
  in trace(show magnitude ++ " * " ++ show (zip3 sites' concs directs) ++ " : " ++ pretty effect) $ if not $ isnull magnitude then magnitude |> effect else vectZero
  --  if any (not.isnull) concs then law concs |> react directs else vectZero
  | ConcreteAffinity (RateLaw law) sites <- network ]
  where Vect h     = potential
        potential' = Vect $ H.filter (not.isnull) h

partial' :: (Vector k (InteractionVect k), Expression k) => (Species -> MTS) -> ProcessVect k -> InteractionVect k
partial' tr = (partial'' ><)
  where partial'' spec = fromList [(1, s :* s' :* L.sort a)
                           | Trans s a s' <- simplify $ tr spec]

-- dPdt without specifying how to compute reactions
dPdt' :: (Vector k (ProcessVect k), Show k, DoubleExpression k) => (Species -> MTS) -> ConcreteAffinityNetwork -> ProcessVect k -> ProcessVect k
dPdt' = dPdt'' react

dPdt'' :: (Vector k (ProcessVect k), Show k, DoubleExpression k, Pretty v, Vector k v) => ([DirectionVect k] -> v) -> (Species -> MTS) -> ConcreteAffinityNetwork -> ProcessVect k -> v
dPdt'' react' tr network p = trace ("part = " ++ pretty part ++ ", acts = " ++ pretty acts) acts
  where acts = actions' react' network part
        part = partial' tr p

-- Extends the support of a process vector to the closure,
-- which contains all the support of potentially reachable processes
closure :: (Species -> MTS) -> ConcreteAffinityNetwork -> ProcessVect BoolConc -> ProcessVect BoolConc
closure tr network v
  | u == v    = v
  | otherwise = closure tr network u
  where v' = dPdt' tr network v
        u  = v +> v'

-- Helpers for handling models

tracesGivenNetwork :: ConcreteAffinityNetwork -> Env -> Species -> MTS
tracesGivenNetwork network = trace ("prefLists = " ++ show prefLists) (transFiltered validPref)
  where
    prefLists = L.nub $ L.sort $ map (L.sort . map prefName) $ L.concatMap (map (map Unlocated) . cAffSites) network
    prefListSubsets = L.nub $ L.sort $ L.concatMap powerset prefLists
    validPref :: PrefixFilter
    validPref x = trace ("testing potential " ++ show x) (L.sort x `elem` prefListSubsets)

concretifyAffSpec :: BondCalculusModel
                  -> AffinityNetworkSpec
                  -> Either String ConcreteAffinityNetwork
concretifyAffSpec model@(Defs _ a _ _) (AffinityNetworkAppl name rates)
  = case M.lookup name a of
      Just (AffinityNetworkDef params body) ->
        if length rates == length rates
        then networkFromList [applyAff model aff params rates | aff <- body]
        else Left $ "Affinity network " ++ name ++ " applied to the "
                     ++ "wrong number of arguments."
      Nothing -> Left $ "Affinity network " ++ name ++ " does not exist."
concretifyAffSpec model (AffinityNetworkSpec network) =
  networkFromList [applyAff model aff [] [] | aff <- network]
concretifyAffSpec model (AffinityNetworkCompo x y) =
  liftM2 (++) (concretifyAffSpec model x) (concretifyAffSpec model y)

networkFromList :: [Either String ConcreteAffinity]
                -> Either String ConcreteAffinityNetwork
networkFromList affsOrErrors = case errors of
                                []    -> Right affs
                                (e:_) -> Left e
  where errors = [e | Left e <- affsOrErrors]
        affs   = [aff | Right aff <- affsOrErrors]

applyAff :: BondCalculusModel -> Affinity -> [String] -> [Rate] -> Either String ConcreteAffinity
applyAff model (Affinity rls sites) params rates
  = second (`ConcreteAffinity` map L.sort sites) $ applyRateLawSpec model rls params rates

applyRateLawSpec :: BondCalculusModel
                 -> RateLawSpec
                 -> [String]
                 -> [Rate]
                 -> Either String RateLaw
applyRateLawSpec (Defs _ _ m _) (RateLawAppl name params) affParams rates
  = case M.lookup name m of
      Nothing -> Left $ "Kinetic law " ++ name ++ " does not exist."
      Just rlf -> if any isNothing args
                  then Left $ "Kinetic law " ++ name ++ " applied to "
                               ++ "variables which do not exist."
                  else Right $ rlf $ map (fromMaybe undefined) args
    where vals = zip affParams rates
          applyRate (RateLawParamVal x) = Just x
          applyRate (RateLawParamVar n) = L.lookup n vals
          args = map applyRate params

combineAndLookupProcess :: BondCalculusModel
                        -> AbstractProcess
                        -> Either String (AffinityNetworkSpec, [(Conc, Species)])
combineAndLookupProcess model@(Defs _ _ _ m) (ProcessAppl x) = do
  p <- maybeToEither ("Process \"" ++ x ++ "\" not defined!") (M.lookup x m)
  combineAndLookupProcess model p
combineAndLookupProcess model (ProcessCompo p q) = liftM2 (<>)
  (combineAndLookupProcess model p) (combineAndLookupProcess model q)
combineAndLookupProcess _ (Process affSpec concSpecs) = return (affSpec, concSpecs)

concretifyProcess :: BondCalculusModel
                  -> Env
                  -> AbstractProcess
                  -> Either String ConcreteDef
concretifyProcess model env proc = do
  (affSpec, concSpecs) <- combineAndLookupProcess model proc
  network <- concretifyAffSpec model affSpec
  return (network, tracesGivenNetwork network env,
          fromList [(c, normalForm s) | (c,s) <- concSpecs])

symbolifyProcess :: BondCalculusModel
                 -> Env
                 -> AbstractProcess
                 -> Either String SymbolicDef
symbolifyProcess model env proc = do
  (affSpec, concSpecs) <- combineAndLookupProcess model proc
  network <- concretifyAffSpec model affSpec
  -- This code takes some care to keep the lists/vector bases in a consistient
  -- order, matching the ordering on basis elements (prime species),
  -- but is presently, sadly, rather unreadable
  let p0 = fromList concSpecs
      getInit i = sum [c | (c, s) <- concSpecs, s == i]
      reach = closure tr network (support p0)
      concSpecs' = [(getInit i, i) | (x, i) <- toList reach, x == NonZero]
      p     = fromList $ map (\(_,x,y) -> (x,y)) ress
      tr = tracesGivenNetwork network env
      ress = sortWith (\(_,_,x) -> x)
                      [let nf = normalForm x
                           name = var (pretty nf)
                       in (x0, name, nf) | (x0,x) <- concSpecs']
      inits = map (\(x,_,_) -> x) ress
  return (network, tr, p, inits)

symbolifyModel :: BondCalculusModel -> Either String SymbolicModel
symbolifyModel model@(Defs env _ _ p)
  = case errors of
      []            -> Right (env, processes)
      ((name, e):_) -> Left $ "Error found in process definition " ++ name
                               ++ ":\n" ++ e
  where processes = M.fromList [ (name, process) | (name, Right process) <- cprocs]
        cprocs    = M.toList $ fmap (symbolifyProcess model env) p
        errors    = [(name, e) | (name, Left e) <- cprocs]

concretifyModel :: BondCalculusModel -> Either String ConcreteModel
concretifyModel model@(Defs env _ _ p)
  = case errors of
      []            -> Right (env, processes)
      ((name, e):_) -> Left $ "Error found in process definition " ++ name
                               ++ ":\n" ++ e
  where processes = M.fromList [ (name,  process)
                    | (name, Right process) <- cprocs]
        cprocs    = M.toList $ fmap (concretifyProcess model env) p
        errors    = [(name, e) | (name, Left e) <- cprocs]

networkSites :: ConcreteAffinityNetwork -> [[Prefix]]
networkSites = L.concatMap (map (map Unlocated) . cAffSites)

partial :: Env -> Process -> D
partial env (Mixture ((c,spec):xs)) = fromList
                                      [(c, s :* s' :* a)
                                      | Trans s a s' <- simplify $ trans env spec]
                                      +> partial env (Mixture xs)
partial env (React network p) = hide (networkSites network) $ partial env p
partial _ (Mixture []) = vectZero

speciesVar :: Species -> SymbolicExpr
speciesVar s = var $ "[" ++ pretty (normalForm s) ++ "]"

embedProcess :: Process -> P
embedProcess (Mixture ps) = fromList ps

dPdt :: Env -> Process -> P
dPdt _ (Mixture _) = vectZero
dPdt env (React network p) = dPdt' tr network pro
  where pro = embedProcess p
        -- tr  = trans env
        tr = tracesGivenNetwork network env
-- TODO: nested Mixtures inside Reacts
