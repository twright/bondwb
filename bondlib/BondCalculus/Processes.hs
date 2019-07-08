{-# LANGUAGE IncoherentInstances #-}
module BondCalculus.Processes (Process(..), Affinity(..), ProcessVect,
  InteractionVect, DirectionVect, ConcreteAffinityNetwork, SymbolicModel,
  SymbolicDef, ConcreteModel, ConcreteDef, P, D, P', D',
  concretifyModel, partial, conc, direct, react, hide, networkSites,
  actions, dPdt, partial', dPdt', dPdt'', dPdt''', tracesGivenNetwork, speciesVar,
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
import BondCalculus.Symbolic (SymbolicExpr, ExprConstant, Expr(..), Atom(..), var)
import Data.Maybe
import Data.Either
import Data.Either.Utils
import Data.Bifunctor
import Control.Monad
-- import Debug.Trace
import GHC.Exts (sortWith)
trace :: String -> a -> a
trace _ = id

type SymbConc a = SymbolicExpr a

-- Process vectors over k
type ProcessVect k = Vect Species k
-- Interaction vectors over k
type InteractionVect k = Vect (Tensor (Tensor Species Abstraction) [Prefix]) k
-- Direction vectors over k
type DirectionVect k = Vect (Tensor Species Abstraction) k

-- NOTE: concrete versions are use doubles as concentrations
-- (we do not automatically apply numerical semantics to intervals)
-- Process space
-- type Conc = Double
type P = ProcessVect Conc
-- Potential interaction space
type D = InteractionVect Conc

-- Process space
type P' a = ProcessVect (SymbConc a)
-- Potential interaction space
type D' a = InteractionVect (SymbConc a)

data Process = Mixture [(Conc, Species)]
             | React (ConcreteAffinityNetwork Conc) Process

-- A BondCalculus model with all of the variables looked up
type ConcreteDef = (ConcreteAffinityNetwork Conc, Species -> MTS, P)
type ConcreteModel = (Env, M.Map String ConcreteDef)
type SymbolicDef a = (ConcreteAffinityNetwork a, Species -> MTS, P' a, [a])
type SymbolicModel a = (Env, M.Map String (SymbolicDef a))

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

actions :: (ExpressionOver m k, Vector k (DirectionVect k), Nullable (DirectionVect k), DoubleExpression k, Show k) =>
           ConcreteAffinityNetwork m -> InteractionVect k -> ProcessVect k
actions = actions' react

-- actions, but without specifying how to compute reactions
actions' :: (ExpressionOver m k, Vector k (DirectionVect k), Nullable (DirectionVect k), DoubleExpression k, Vector k v, Show k, Pretty v) =>
           ([DirectionVect k] -> v) -> ConcreteAffinityNetwork m -> InteractionVect k -> v
actions' react' network !potential = L.foldl' (+>) vectZero [
  let concs   = map (`conc` potential') sites'
      directs = map (`direct` potential') sites'
      sites'  = map (L.sort . map Unlocated) sites
      magnitude = val $ law concs
      effect  = react' directs
  in trace(show magnitude ++ " * " ++ show (zip3 sites' concs directs) ++ " : " ++ pretty effect) $ if not $ isnull magnitude then magnitude |> effect else vectZero
  --  if any (not.isnull) concs then law concs |> react directs else vectZero
  | ConcreteAffinity (RateLaw law) sites <- network ]
  where Vect h     = potential
        potential' = Vect $ H.filter (not.isnull) h

-- actions'D :: forall v . (Pretty v, Vector (SymbolicExpr Double) v) => ([DirectionVect (SymbolicExpr Double)] -> v) -> ConcreteAffinityNetwork Double -> InteractionVect (SymbolicExpr Double) -> v
-- actions'D react' (network :: ConcreteAffinityNetwork Double) !potential = L.foldl' (+>) vectZero [
--   let concs   = map (`conc` potential') sites'
--       directs = map (`direct` potential') sites'
--       sites'  = map (L.sort . map Unlocated) sites
--       magnitude = (law :: RateLawFn Double) concs
--       effect  = react' directs
--   in trace(show magnitude ++ " * " ++ show (zip3 sites' concs directs) ++ " : " ++ pretty effect) $ if not $ isnull magnitude then magnitude |> effect else vectZero
--   --  if any (not.isnull) concs then law concs |> react directs else vectZero
--   | ConcreteAffinity (RateLaw law) sites <- network ]
--   where Vect h     = potential
--         potential' = Vect $ H.filter (not.isnull) h

partial' :: (Vector k (InteractionVect k), Expression k) => (Species -> MTS) -> ProcessVect k -> InteractionVect k
partial' tr = (partial'' ><)
  where partial'' spec = fromList [(1, s :* s' :* L.sort a)
                           | Trans s a s' <- simplify $ tr spec]

-- dPdt with most liberal type (including symbolic expressions and affinity network 
-- coercible to process concentration type)
dPdt' :: (ExpressionOver m k, Vector k (ProcessVect k), Show k, DoubleExpression k, DoubleExpression m)
      => (Species -> MTS) -> ConcreteAffinityNetwork m -> ProcessVect k -> ProcessVect k
dPdt' = dPdt'' react

-- dPdt with slightly more restricted type (including symbolic expressions and affinity network 
-- same type as vector)
dPdt''' :: (Vector k (ProcessVect k), Show k, DoubleExpression k)
        => (Species -> MTS) -> ConcreteAffinityNetwork k -> ProcessVect k -> ProcessVect k
dPdt''' = dPdt'' react

-- abstract dPdt without specifying how to compute reactions yet
dPdt'' :: (ExpressionOver m k, Vector k (ProcessVect k), Show k, DoubleExpression k, Pretty v, Vector k v)
       => ([DirectionVect k] -> v)
       -> (Species -> MTS) -> ConcreteAffinityNetwork m -> ProcessVect k -> v
dPdt'' react' tr network p = trace ("part = " ++ pretty part ++ ", acts = " ++ pretty acts) acts
  where acts = actions' react' network part
        part = partial' tr p

-- dPdt''D :: forall k v .
--            (Vector k (ProcessVect k), Show k, DoubleExpression k, Pretty v, Vector k v)
--         => ([DirectionVect k] -> v)
--         -> (Species -> MTS) -> ConcreteAffinityNetwork Double -> ProcessVect k -> v
-- dPdt''D react' tr network p = trace ("part = " ++ pretty part ++ ", acts = " ++ pretty acts) acts
--   where acts :: v
--         acts = actions'D react' network part
--         part :: InteractionVect k 
--         part = partial' tr p

-- Extends the support of a process vector to the closure,
-- which contains all the support of potentially reachable processes
closure :: (ExpressionOver a BoolConc, DoubleExpression a) => (Species -> MTS) -> ConcreteAffinityNetwork a -> ProcessVect BoolConc -> ProcessVect BoolConc
closure tr network v
  | u == v    = v
  | otherwise = closure tr network u
  where v' = dPdt' tr network v
        u  = v +> v'

-- Helpers for handling models

tracesGivenNetwork :: ConcreteAffinityNetwork a -> Env -> Species -> MTS
tracesGivenNetwork network = trace ("prefLists = " ++ show prefLists) (transFiltered validPref)
  where
    prefLists = L.nub $ L.sort $ map (L.sort . map prefName) $ L.concatMap (map (map Unlocated) . cAffSites) network
    prefListSubsets = L.nub $ L.sort $ L.concatMap powerset prefLists
    validPref :: PrefixFilter
    validPref x = trace ("testing potential " ++ show x) (L.sort x `elem` prefListSubsets)

concretifyAffSpec :: BondCalculusModel a
                  -> AffinityNetworkSpec a
                  -> Either String (ConcreteAffinityNetwork a)
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

networkFromList :: [Either String (ConcreteAffinity a)]
                -> Either String (ConcreteAffinityNetwork a)
networkFromList affsOrErrors = case errors of
                                []    -> Right affs
                                (e:_) -> Left e
  where errors = [e | Left e <- affsOrErrors]
        affs   = [aff | Right aff <- affsOrErrors]

applyAff :: BondCalculusModel a -> Affinity a -> [String] -> [a] -> Either String (ConcreteAffinity a)
applyAff model (Affinity rls sites) params rates
  = second (`ConcreteAffinity` map L.sort sites) $ applyRateLawSpec model rls params rates

applyRateLawSpec :: BondCalculusModel a
                 -> RateLawSpec a
                 -> [String]
                 -> [a] -- the rates
                 -> Either String (RateLaw a)
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

combineAndLookupProcess :: BondCalculusModel a
                        -> AbstractProcess a
                        -> Either String (AffinityNetworkSpec a, [(a, Species)])
combineAndLookupProcess model@(Defs _ _ _ m) (ProcessAppl x) = do
  p <- maybeToEither ("Process \"" ++ x ++ "\" not defined!") (M.lookup x m)
  combineAndLookupProcess model p
combineAndLookupProcess model (ProcessCompo p q) = liftM2 (<>)
  (combineAndLookupProcess model p) (combineAndLookupProcess model q)
combineAndLookupProcess _ (Process affSpec concSpecs) = return (affSpec, concSpecs)

concretifyProcess :: BondCalculusModel Conc
                  -> Env
                  -> AbstractProcess Conc
                  -> Either String ConcreteDef
concretifyProcess model env proc = do
  (affSpec, concSpecs) <- combineAndLookupProcess model proc
  network <- concretifyAffSpec model affSpec
  return (network, tracesGivenNetwork network env,
          fromList [(c, normalForm s) | (c,s) <- concSpecs])

symbolifyProcess :: ExprConstant a =>
                    BondCalculusModel a
                 -> Env
                 -> AbstractProcess a
                 -> Either String (SymbolicDef a)
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

symbolifyModel :: ExprConstant a =>
                  BondCalculusModel a
               -> Either String (SymbolicModel a)
symbolifyModel model@(Defs env _ _ p)
  = case errors of
      []            -> Right (env, processes)
      ((name, e):_) -> Left $ "Error found in process definition " ++ name
                               ++ ":\n" ++ e
  where processes = M.fromList [ (name, process) | (name, Right process) <- cprocs]
        cprocs    = M.toList $ fmap (symbolifyProcess model env) p
        errors    = [(name, e) | (name, Left e) <- cprocs]

concretifyModel :: BondCalculusModel Double -> Either String ConcreteModel
concretifyModel model@(Defs env _ _ p)
  = case errors of
      []            -> Right (env, processes)
      ((name, e):_) -> Left $ "Error found in process definition " ++ name
                               ++ ":\n" ++ e
  where processes = M.fromList [ (name,  process)
                    | (name, Right process) <- cprocs]
        cprocs    = M.toList $ fmap (concretifyProcess model env) p
        errors    = [(name, e) | (name, Left e) <- cprocs]

networkSites :: ConcreteAffinityNetwork a -> [[Prefix]]
networkSites = L.concatMap (map (map Unlocated) . cAffSites)

partial :: Env -> Process -> D
partial env (Mixture ((c,spec):xs)) = fromList
                                      [(c, s :* s' :* a)
                                      | Trans s a s' <- simplify $ trans env spec]
                                      +> partial env (Mixture xs)
partial env (React network p) = hide (networkSites network) $ partial env p
partial _ (Mixture []) = vectZero

speciesVar :: Species -> SymbolicExpr a
speciesVar s = var $ "[" ++ pretty (normalForm s) ++ "]"

embedProcess :: Process -> P
embedProcess (Mixture ps) = fromList ps

dPdt :: Env -> Process -> P
-- dPdt = undefined
dPdt _ (Mixture _) = vectZero
dPdt env (React network p) = dPdt''' tr network pro
  where pro = embedProcess p 
        n' = network
        -- tr  = trans env

        tr = tracesGivenNetwork network env
        -- dPdt''' :: (Species -> MTS) -> ConcreteAffinityNetwork Double -> ProcessVect Double -> ProcessVect Double
        -- dPdt''' = dPdt'
-- TODO: nested Mixtures inside Reacts
