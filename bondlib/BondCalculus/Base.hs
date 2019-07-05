{-# LANGUAGE ExistentialQuantification, RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module BondCalculus.Base
  (Expression(..), Pretty(..), Nullable(..), DoubleExpression(..), BoolConc(..), Interval(..),
   Boundable(..), fromEndpoints, endpoints, singleValue, powerset) where

import Text.Printf
import AERN2.MP ()
import AERN2.MP.Precision
import qualified AERN2.MP.Ball as MPB
import qualified AERN2.MP.Float as MPF

import Debug.Trace

-- utils

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

-- Pretty printing
class (Show a) => Pretty a where
  pretty :: a -> String
  pretty = show

class Expression a where
  simplify :: a -> a
  simplify = id

class (Num a, Floating a, Expression a) => DoubleExpression a where
  fromFloat :: Double -> a
  fromInterval :: Double -> Double -> a
  fromInterval x y = trace
                     (printf "WARNING: casting interval %s to double "
                              (show $ fromEndpoints (toRational x) (toRational y))) $
                     fromFloat $ (x + y) / 2

class Boundable a where
    bounds :: a -> (Double, Double)
    inf :: a -> Double
    inf = fst.bounds
    sup :: a -> Double
    sup = snd.bounds

singleValue :: Boundable a => a -> Maybe Double
singleValue x | l == u    = Just l
              | otherwise = Nothing
    where (l, u) = bounds x

instance Boundable Double where
    bounds x = (x, x)

instance Boundable Interval where
    bounds = endpoints

instance Expression Double where
instance Expression String where
instance Expression Integer where
instance DoubleExpression Double where
  fromFloat = id

data BoolConc = Zero | NonZero deriving (Eq, Show)

instance Expression BoolConc where
instance Num BoolConc where
  NonZero + _ = NonZero
  _ + NonZero = NonZero
  Zero + Zero = Zero
  Zero * _ = Zero
  _ * Zero = Zero
  NonZero * NonZero = NonZero
  abs = id
  fromInteger 0 = Zero
  fromInteger _ = NonZero
  negate = id
  signum Zero = 0
  signum NonZero = 1
instance Fractional BoolConc where
  NonZero / NonZero = NonZero
  Zero / NonZero = Zero
  _ / Zero = NonZero
  fromRational 0 = Zero
  fromRational _ = NonZero
instance Floating BoolConc where
  pi = NonZero
  sin Zero = Zero
  sin NonZero = NonZero
  tan Zero = Zero
  tan NonZero = NonZero
  exp = const NonZero
  cos = const NonZero
  asin = const NonZero
  acos = const NonZero
  atan = const NonZero
  sinh = const NonZero
  cosh = const NonZero
  asinh = const NonZero
  acosh = const NonZero
  atanh = const NonZero
  log = const NonZero
instance DoubleExpression BoolConc where
  fromFloat 0 = Zero
  fromFloat _ = NonZero
instance Nullable BoolConc where
  isnull = (==) Zero

class Nullable a where
  isnull :: a -> Bool

instance Nullable Double where
  isnull = (<1e-16).abs

-- AERN2 based intervals -- MPBall
newtype Interval = Interval MPB.MPBall deriving (Ord)

fromEndpoints :: Rational -> Rational -> Interval
fromEndpoints l u = Interval $ MPB.fromEndpoints l' u'
    where l' = MPF.fromRationalUp defaultPrecision l
          u' = MPF.fromRationalDown defaultPrecision u

-- NOTE: toDouble rounds upwards so this is conservative
endpoints :: Interval -> (Double, Double)
endpoints (Interval x) = (MPF.toDoubleDown l, MPF.toDoubleUp u)
    where (l, u) = MPB.endpoints x :: (MPF.MPFloat, MPF.MPFloat)

instance Eq Interval where
    x == y = endpoints x == endpoints y

instance Show Interval where
    show x = printf "[%.6f .. %.6f]" l u
        where (l, u) = endpoints x

instance Expression Interval where
instance DoubleExpression Interval where
    fromFloat = fromRational . toRational
    fromInterval x y = fromEndpoints (toRational x) (toRational y)

instance Num Interval where
  Interval x + Interval y = Interval (x + y)
  Interval x * Interval y = Interval (x * y)
  abs (Interval x) = Interval (abs x)
  fromInteger x = Interval (fromInteger x)
  negate (Interval x) = Interval (negate x)
  signum (Interval x) = Interval (signum x)
instance Fractional Interval where
  Interval x / Interval y = Interval (x / y)
  fromRational x = fromEndpoints x x
instance Floating Interval where
  pi = Interval pi
  sin (Interval x) = Interval (sin x)
  cos (Interval x) = Interval (cos x)
  tan (Interval x) = Interval (tan x)
  exp (Interval x) = Interval (exp x)
  asin (Interval x) = error "not implemented"
  acos (Interval x) = error "not implemented"
  atan (Interval x) = error "not implemented"
  sinh (Interval x) = error "not implemented"
  cosh (Interval x) = error "not implemented"
  asinh (Interval x) = error "not implemented"
  acosh (Interval x) = error "not implemented"
  atanh (Interval x) = error "not implemented"
  log (Interval x) = Interval (log x)
instance Nullable Interval where
  isnull = (== fromRational 0) . abs