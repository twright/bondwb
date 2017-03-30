{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleContexts, FunctionalDependencies #-}

module CPi.Vector (Tensor(..), Vector(..), Vect(..), (><), vect, (*>), delta, fromList, multilinear) where

import Prelude hiding ((*>))

import Data.Hashable
import Data.HashMap.Strict hiding (fromList)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L

-- Module inspired by Jan Skibinski's Quantum Vector module

infixl 7 *>  -- tensor product of two kets
infix 6 |> -- scale a vector by a constant
infixl 5 +>  -- sum of two kets
infix 4 <>  -- inner product
infix 5 ><  -- closure

-- (Num b, Hashable a) =>
newtype (Num k, Hashable i) => Vect i k = Vect (HashMap i k)
  deriving Functor
-- newtype Bra = (Num b, Hashable a) => Map a b

class (Num k) => Vector k v | v -> k where
  -- (*>)       :: v -> v -> v
  (+>)       :: v -> v -> v
  (|>)       :: k -> v -> v
  reduce     :: v -> v
  basis      :: v -> [v]
  components :: v -> [k]
  -- compose    :: [k] -> [v] -> v
  dimension  :: v -> Int
  norm       :: v -> k
  normalize  :: v -> v
  isnull     :: v -> Bool
  vectZero   :: v
  (<>)       :: v -> v -> k

-- A fancified tuple for forming cartesian products of basis elements
data Tensor a b =  a :* b
  deriving (Eq, Ord)

instance (Hashable a, Hashable b) => Hashable (Tensor a b) where
  hashWithSalt i (u :* v) = i `hashWithSalt` u `hashWithSalt` v

instance (Show a, Show b) => Show (Tensor a b) where
  showsPrec n (a :* b) = showsPrec n a . showString "; " . showsPrec n b

instance (Eq k, Num k, Fractional k, Eq i, Hashable i) => Vector k (Vect i k) where
  vectZero = Vect H.empty
  (|>) a = fmap (*a)
  Vect u +> Vect v = Vect (unionWith (+) u v)
  reduce (Vect v) = Vect (H.filter (/=0) v)
  basis (Vect u) = [Vect (singleton v 1) | v <- keys u]
  components (Vect u) = elems u
  dimension (Vect u) = size u
  -- compose ks vs = L.foldl' (+>) (Vect H.empty) $ zipWith (|>) ks vs
  norm (Vect v) = H.foldl' ((+).abs) 0 v
  normalize v@(Vect m)
    | normv == 0 = v
    | otherwise  = Vect (H.map (/normv) m)
    where normv = norm v
  isnull = (==0) . norm
  Vect u <> Vect v = H.foldl' (+) 0 $ H.intersectionWith (*) u v
  -- u <> v = (\i -> (\j -> delta i j) >< v) >< u
fromList :: (Eq k, Num k, Fractional k, Eq i, Hashable i) => [(k,i)] -> Vect i k
fromList kvs = Vect $ H.fromListWith (+) $ L.map (\(i,v) -> (v,i)) kvs

instance (Eq k, Num k, Fractional k, Eq i, Hashable i) => Eq (Vect i k) where
  x == y = isnull (x +> (-1) |> y)

vect :: (Eq k, Num k, Fractional k, Eq i, Hashable i) => i -> Vect i k
vect i = Vect $ singleton i 1

-- expands a function defined on a basis to one on vectors
(><) :: (Eq k, Num k, Fractional k, Eq i, Hashable i, Eq j, Hashable j) =>
        (i -> Vect j k) -> Vect i k -> Vect j k
f >< Vect v = L.foldl' (+>) (Vect H.empty) [a |> f i | (i,a) <- H.toList v]

-- expands a funtion defined on lists of basis elements, to a multilinear function on lists of vectors
multilinear :: (Eq k, Num k, Fractional k, Eq i, Hashable i, Eq j, Hashable j) =>
              ([i] -> Vect j k) -> [Vect i k] -> Vect j k
multilinear f (v:vs) = (\i -> multilinear (\is -> f (i:is)) vs) >< v
-- multilinear f [v] = f >< v
multilinear f [] = f []
-- f >< Vect v = L.foldl' (+>) (Vect H.empty) [a |> f i | (i,a) <- H.toList v]

(*>) :: (Eq k, Num k, Fractional k, Eq i, Hashable i, Eq j, Hashable j) =>
        Vect i k -> Vect j k -> Vect (Tensor i j) k
u *> v = (\i -> (\j -> vect (i :* j)) >< v) >< u

delta :: Eq a => a -> a -> Integer
delta i j | i == j = 1
          | otherwise = 0

instance (Num k, Hashable i, Eq i, Eq k, Fractional k, Show i, Show k) => Show (Vect i k) where
  show v@(Vect l)
    | isnull v = "vectZero"
    | otherwise = L.intercalate " +> "
      [show k ++ " |" ++ show i ++ ">"  | (i,k) <- toList l]


-- (*>) :: (Hashable i, Hashable j) => Vect i k -> Vect j k -> Vect (Tensor i j) k
-- Vect u *> Vect v = Vect (u :* Vect v)
