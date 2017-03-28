{-# LANGUAGE  FlexibleInstances #-}

module CPi.Processes (Process(..), Affinity(..), AffinityNetwork(..),
  P, D, norm1, partial, conc, direct, multilinear, react, hide,
  networkSites, actions, dPdt) where

import Prelude hiding ((*>), (<*))
import CPi.AST
import CPi.Transitions
 -- Yes, we are using bra-ket notation to represent vectors
 -- in an index free manner
import QuantumVector
import Data.Complex
import qualified Data.List as L

-- Process space
type P = Ket Species
-- Potential interaction space
type D = Ket (Tuple (Tuple Species Abstraction) [Prefix])

data Affinity = Affinity { affRateLaw :: RateLaw
                         , affSites   :: [[Prefix]] }

type AffinityNetwork = [Affinity]

data Process = Mixture [(Conc, Species)]
             | React AffinityNetwork Process

norm1 :: (DiracVector a) => a -> Double
norm1 x = sum $ map magnitude $ components x

instance {-# OVERLAPS #-} Eq P where
  x == y = norm1 (x +> (-1.0) |> y) == 0.0

instance {-# OVERLAPS #-} Eq D where
  x == y = norm1 (x +> (-1.0) |> y) == 0.0

normalize1 :: (DiracVector a) => a -> a
normalize1 x | n == 0 = x
             | otherwise = scale (1/n :+ 0.0) x
  where n = norm1 x

conc :: [Prefix] -> D -> Double
conc s = norm1 . (conc' ><)
  where conc' (Ket (_ :* _ :* t)) = d s t |> Ket t

direct :: [Prefix] -> D -> Ket (Tuple Species Abstraction)
direct l = normalize1 . (direct' ><)
  where direct' (Ket (s :* s' :* m)) = d l m |> Ket (s :* s')

networkSites :: AffinityNetwork -> [[Prefix]]
networkSites = L.concatMap affSites

hide :: [[Prefix]] -> D -> D
hide toHide = (hide' ><)
  where hide' e@(Ket (s :* s' :* sites)) | sites `notElem` toHide = e
                                         | otherwise              = KetZero

partial :: Env -> Process -> D
partial env (Mixture ((c,spec):xs)) = (c :+ 0.0) |> foldl (+>) KetZero
                                      [Ket s *> Ket s' *> Ket a
                                      | Trans _ s a s' <- simplify $ trans spec env] +> partial env (Mixture xs)
partial env (React network p) = hide (networkSites network) $ partial env p
partial _ (Mixture []) = KetZero

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

partialFiltered :: PrefixFilter -> Env -> Process -> D
partialFiltered validPref env (Mixture ((c,spec):xs)) = (c :+ 0.0) |> foldl (+>) KetZero
                                      [Ket s *> Ket s' *> Ket a
                                      | Trans _ s a s' <- simplify $ transFiltered validPref spec env] +> partialFiltered validPref env (Mixture xs)
partialFiltered _ env (React network p) = partialFiltered validPref env p
  where prefLists = L.nub $ L.sort $ map (map prefName) $ L.concatMap affSites network
        prefListSubsets = L.nub $ L.sort $ L.concatMap powerset prefLists
        validPref :: PrefixFilter
        validPref Potential = prefListSubsets `seq` (`elem` prefListSubsets)
        validPref Final = prefLists `seq` (`elem` prefLists)
partialFiltered _ _ (Mixture []) = KetZero

-- multilinear extension of a function
multilinear :: (Ord a, Ord b) => ([Ket a] -> Ket b) -> [Ket a] -> Ket b
multilinear f = multilinear' f []
  where multilinear' f ys [] = f (reverse ys)
        multilinear' f ys (x:xs) = foldl (+>) KetZero terms
          where es     = basis x
                alphas = components x
                terms  = [alpha |> multilinear' f (e:ys) xs
                         | (alpha, e) <- zip alphas es]

primes :: Species -> [Species]
primes (Par ss) = L.concatMap primes ss
primes s = [s]

embed :: Species -> Ket Species
embed spec = foldl (+>) KetZero $ map Ket $ filter (/=Nil) $ primes $ normalForm spec

react :: [Ket (Tuple Species Abstraction)] -> P
react = multilinear react'
  where react' xs = embed (concretify $
                    foldl (<|>) (AbsBase Nil) (map target xs)) +>
                    (-1.0) |> foldl (+>) KetZero (map (embed.source) xs)
        source (Ket (spec :* spec')) = spec
        target (Ket (spec :* spec')) = spec'

actions :: AffinityNetwork -> D -> P
actions network potential = foldl (+>) KetZero [
  let concs   = map (`conc` potential) sites'
      directs = map (`direct` potential) sites'
      sites'  = map L.sort sites
  in (law concs :+ 0.0) |> react directs
  | Affinity law sites <- network ]

dPdt :: Env -> Process -> P
dPdt env (Mixture ps) = KetZero
dPdt env l@(React network p) = actions network $ partialFiltered (\_ _ -> False) env l
-- TODO: nested Mixtures inside Reacts
-- foldl1 (+>) KetZero [()] (map (dPdt env) ps)

-- react' :: [(Species, Species)] -> P
-- react' specs = Ket
