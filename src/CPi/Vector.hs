{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}

module CPi.Vector (Tensor(..), Vector(..), Vect(..), (><), vect, (*>), delta, toList, fromList, multilinear, support) where

import Prelude hiding ((*>))
import CPi.Base
import Data.Hashable
import Data.HashMap.Strict hiding (fromList, toList)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import GHC.Exts (sortWith)

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
--
-- instance (Eq k, Eq i) => Eq (Vect i k) where
--   x == y = toList x == toList y

class (Eq k, Num k, Fractional k, Nullable k) => Vector k v | v -> k where
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
  vectZero   :: v
  (<>)       :: v -> v -> k

-- Map a vector to an overapproximation of its support set,
-- represented as a 'vector' over boolean concentrations
-- (technically, Vect i BoolConc is a module rather than
-- a vector space)
support :: (Vector k (Vect i k), Hashable i, Ord i) => Vect i k -> Vect i BoolConc
support v = fromList [(NonZero, i) | (k,i) <- toList v, not (isnull k)]

-- A fancified tuple for forming cartesian products of basis elements
data Tensor a b =  a :* b
  deriving (Eq, Ord)

instance (Expression a, Expression b) => Expression (Tensor a b) where
  simplify (a :* b) = simplify a :* simplify b

instance (Hashable a, Hashable b) => Hashable (Tensor a b) where
  hashWithSalt i (u :* v) = i `hashWithSalt` u `hashWithSalt` v

instance (Show a, Show b) => Show (Tensor a b) where
  showsPrec n (a :* b) = showsPrec n a . showString "; " . showsPrec n b

instance (Pretty a, Pretty b) => Pretty (Tensor a b) where
  pretty (a :* b) = pretty a ++ "; " ++ pretty b

-- instance {-# OVERLAPPABLE #-} (Eq k, Num k, Fractional k, Eq i, Hashable i) => Nullable (Vect i k) where
--   isnull = (==0) . norm

instance (Nullable k, Vector k (Vect i k)) => Nullable (Vect i k) where
  isnull = isnull . norm

instance (Eq k, Num k, Nullable k, Fractional k, Eq i, Hashable i) => Vector k (Vect i k) where
  vectZero = Vect H.empty
  (|>) a = fmap (*a)
  Vect u +> Vect v = Vect (unionWith (+) u v)
  reduce (Vect v) = Vect (H.filter (/=0) v)
  basis (Vect u) = [Vect (singleton v 1) | v <- keys u]
  components (Vect u) = elems u
  dimension (Vect u) = size u
  -- compose ks vs = L.foldl' (+>) (Vect H.empty) $ zipWith (|>) ks vs
  norm v = sum (L.map abs (components v))
  normalize v@(Vect m)
    | isnull v = vectZero
    | otherwise  = Vect (H.map (/normv) m)
    where normv = norm v
  Vect u <> Vect v = H.foldl' (+) 0 $ H.intersectionWith (*) u v

instance (Expression k, Expression i, Vector k (Vect i k), Hashable i, Eq i) => Expression (Vect i k) where
  simplify (Vect v) = reduce $ fromList [(simplify k, simplify i)
                                        | (i, k) <- H.toList v]

-- instance (Eq i, Hashable i) => Vector Double (Vect i Double) where

  -- u <> v = (\i -> (\j -> delta i j) >< v) >< u
fromList :: (Eq k, Num k, Fractional k, Eq i, Hashable i, Nullable k) => [(k,i)] -> Vect i k
fromList kvs = Vect $ H.fromListWith (+) $ L.map (\(i,v) -> (v,i)) kvs

toList :: (Num k, Hashable i, Ord i) => Vect i k -> [(k,i)]
toList (Vect v) = sortWith snd $ fmap (\(x,y) -> (y,x)) (H.toList v)

instance (Eq k, Ord i, Num k, Fractional k, Expression k, Expression i, Eq i, Hashable i, Nullable k) => Eq (Vect i k) where
  x == y = kvs x == kvs y
    where kvs w = L.filter (not.isnull.fst) $ toList $ fmap simplify w

vect :: (Eq k, Num k, Fractional k, Eq i, Hashable i) => i -> Vect i k
vect i = Vect $ singleton i 1

-- expands a function defined on a basis to one on vectors
(><) :: (Eq k, Num k, Fractional k, Eq i, Hashable i, Eq j, Hashable j, Nullable k) =>
        (i -> Vect j k) -> Vect i k -> Vect j k
f >< Vect v = L.foldl' (+>) (Vect H.empty) [a |> f i | (i,a) <- H.toList v]

-- expands a funtion defined on lists of basis elements, to a multilinear function on lists of vectors
multilinear :: (Eq k, Num k, Fractional k, Eq i, Hashable i, Eq j, Hashable j, Nullable k) =>
              ([i] -> Vect j k) -> [Vect i k] -> Vect j k
multilinear f (v:vs) = (\i -> multilinear (\is -> f (i:is)) vs) >< v
-- multilinear f [v] = f >< v
multilinear f [] = f []
-- f >< Vect v = L.foldl' (+>) (Vect H.empty) [a |> f i | (i,a) <- H.toList v]

(*>) :: (Eq k, Num k, Fractional k, Eq i, Hashable i, Eq j, Hashable j, Nullable k) =>
        Vect i k -> Vect j k -> Vect (Tensor i j) k
u *> v = (\i -> (\j -> vect (i :* j)) >< v) >< u

delta :: Eq a => a -> a -> Integer
delta i j | i == j = 1
          | otherwise = 0

instance (Num k, Hashable i, Eq i, Eq k, Fractional k, Ord i, Show i, Show k, Nullable k) => Show (Vect i k) where
  show v
    | isnull v = "vectZero"
    | otherwise = L.intercalate " +> "
      [show k ++ " |" ++ show i ++ ">"  | (k,i) <- toList v]

instance (Num k, Hashable i, Eq i, Eq k, Fractional k, Ord i, Pretty i, Show k, Nullable k) => Pretty (Vect i k) where
  pretty v
    | isnull v = "vectZero"
    | otherwise = L.intercalate " +> "
      [show k ++ " |" ++ pretty i ++ ">"  | (k,i) <- toList v]

-- (*>) :: (Hashable i, Hashable j) => Vect i k -> Vect j k -> Vect (Tensor i j) k
-- Vect u *> Vect v = Vect (u :* Vect v)
