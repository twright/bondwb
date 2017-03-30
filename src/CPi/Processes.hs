{-# LANGUAGE  FlexibleInstances, BangPatterns #-}

module CPi.Processes (Process(..), Affinity(..), AffinityNetwork(..),
  P, D, partial, conc, direct, react, hide,
  networkSites, actions, dPdt, partial', partialFiltered, dPdt') where

import Prelude hiding ((*>), (<*))
import CPi.AST
import CPi.Transitions
 -- Yes, we are using bra-ket notation to represent vectors
 -- in an index free manner
-- import QuantumVector-- hiding ((><))
import qualified Data.List as L
import qualified Data.HashMap.Strict as H
import CPi.Vector

-- f >< x = L.foldl' (+>) KetZero $ zipWith (|>) (components x) (map f (basis x))

-- Process space
type P = Vect Species Conc
-- Potential interaction space
type D = Vect (Tensor (Tensor Species Abstraction) [Prefix]) Conc

data Affinity = Affinity { affRateLaw :: RateLaw
                         , affSites   :: [[Prefix]] }

type AffinityNetwork = [Affinity]

data Process = Mixture [(Conc, Species)]
             | React AffinityNetwork Process

-- norm1 :: (DiracVector a) => a -> Double
-- norm1 x = sum $ map magnitude $ components x

-- instance {-# OVERLAPS #-} Eq P where
--   x == y = norm1 (x +> (-1.0) |> y) == 0.0
--
-- instance {-# OVERLAPS #-} Eq D where
--   x == y = norm1 (x +> (-1.0) |> y) == 0.0

-- normalize1 :: (DiracVector a) => a -> a
-- normalize1 x | n == 0 = x
--              | otherwise = scale (1/n :+ 0.0) x
--   where n = norm1 x

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

partialFiltered :: PrefixFilter -> Env -> Process -> D
partialFiltered validPref env (Mixture ((c,spec):xs))
  = fromList [(c, s :* s' :* a) | Trans _ s a s' <- simplify $ transFiltered validPref env spec]
    +> partialFiltered validPref env (Mixture xs)
partialFiltered _ env (React network p) = partialFiltered validPref env p
  where prefLists = L.nub $ L.sort $ map (map prefName) $ L.concatMap affSites network
        prefListSubsets = L.nub $ L.sort $ L.concatMap powerset prefLists
        validPref :: PrefixFilter
        validPref Potential = prefListSubsets `seq` (`elem` prefListSubsets)
        validPref Final = prefLists `seq` (`elem` prefLists)
partialFiltered _ _ (Mixture []) = vectZero

-- multilinear extension of a function
-- multilinear :: (Ord a, Ord b) => ([Ket a] -> Ket b) -> [Ket a] -> Ket b
-- multilinear f = multilinear' f []
--   where multilinear' f !ys [] = f (reverse ys)
--         multilinear' f !ys (x:xs) = L.foldl' (+>) KetZero terms
--           where es     = basis x
--                 alphas = components x
--                 terms  = [alpha |> multilinear' f (e:ys) xs
--                          | (alpha, e) <- zip alphas es]

primes :: Species -> [Species]
primes (Par ss) = L.concatMap primes ss
primes s = [s]

embed :: Species -> P
embed spec = fromList $ map (\s -> (1, s)) $ filter (/=Nil) $ primes $ normalForm spec

react :: [Vect (Tensor Species Abstraction) Conc] -> P
react = multilinear react'
  where react' xs = embed (concretify $
                    foldl (<|>) (AbsBase Nil) (map target xs)) +>
                    -- fromList [(-1, source x) | x <- xs]
                    (-1.0) |> foldl (+>) vectZero (map (embed.source) xs)
        source (spec :* spec') = spec
        target (spec :* spec') = spec'

actions :: AffinityNetwork -> D -> P
actions network !potential = L.foldl' (+>) vectZero [
  let concs   = map (`conc` potential) sites'
      directs = map (`direct` potential) sites'
      sites'  = map L.sort sites
  in concs `seq` directs `seq` law concs |> react directs
  | Affinity law sites <- network ]

-- fromList [(c, s :* s' :* a) | Trans _ s a s' <- simplify $ transFiltered validPref env spec]

partial' :: (Species -> MTS) -> P -> D
partial' tr = (partial'' ><)
  where partial'' spec = fromList [(1, s :* s' :* a)
                           | Trans _ s a s' <- simplify $ tr spec]

dPdt' :: (Species -> MTS) -> AffinityNetwork -> P -> P
dPdt' tr network p = actions network $ partial' tr p

dPdt :: Env -> Process -> P
dPdt env (Mixture ps) = vectZero
dPdt env l@(React network p) = actions network $ partialFiltered (\_ _ -> False) env l
-- TODO: nested Mixtures inside Reacts
-- foldl1 (+>) KetZero [()] (map (dPdt env) ps)

-- react' :: [(Species, Species)] -> P
-- react' specs = Ket
