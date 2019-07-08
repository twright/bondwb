{-# LANGUAGE UndecidableInstances, IncoherentInstances #-}
module BondCalculus.Base
  (Expression(..), Pretty(..), Nullable(..), DoubleExpression(..), ExpressionOver(..), BoolConc(..), Interval(..),
   Boundable(..), pattern SingleValue, fromEndpoints, endpoints, singleValue, powerset) where

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

class ExpressionOver a b where
    val :: a -> b

instance ExpressionOver a a where
    val = id

-- instance DoubleExpression Double where
--     fromFloat = id

instance (Nullable a) => ExpressionOver a BoolConc where
    val x | isnull x  = Zero
          | otherwise = NonZero 

-- all of these instances were a very bad idea since they add extra anonomous
-- type variables whenever instantiated and break things
-- AllowAmbiguousTypes considered harmful!
-- instance forall a b . (DoubleExpression a, ExpressionOver a b) => ExpressionOver Double b where
--     val x = val (fromFloat x :: a)

-- instance forall a b . (Fractional a, ExpressionOver a b) => ExpressionOver Rational b where
--     val x = val (fromRational x :: a)

-- instance {-# OVERLAPPABLE #-} forall a b . (Num a, ExpressionOver a b) => ExpressionOver Integer b where
--     val x = val (fromInteger x :: a)

-- we don't actually want this transantivity rule since it leads to overlaps
-- instance {-# OVERLAPPABLE #-} forall a b c . (ExpressionOver a b, ExpressionOver b c) => ExpressionOver a c where
--     val x = val (val x :: b)

class (Num a, Floating a, Expression a) => DoubleExpression a where
    fromFloat :: Double -> a
    fromInterval :: Double -> Double -> a
    fromInterval x y = trace
                        (printf "WARNING: casting interval %s to double "
                                (show $ fromEndpoints (toRational x) (toRational y))) $
                        fromFloat $ (x + y) / 2

-- instance (ExpressionOver Double k, Num k, Floating k, Expression k) => DoubleExpression k where
--     fromFloat = val

class Boundable a where
    bounds :: a -> (Double, Double)
    inf :: a -> Double
    inf = fst.bounds
    sup :: a -> Double
    sup = snd.bounds

pattern SingleValue x <- (singleValue -> Just x)

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
-- instance Nullable BoolConc where
--   isnull = (==) Zero

class Nullable a where
    isnull :: a -> Bool

-- instance Nullable Double where
--   isnull = (<1e-16).abs

instance (Num a, Eq a) => Nullable a where
    isnull = (==) 0 

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
    asin (Interval _) = error "not implemented"
    acos (Interval _) = error "not implemented"
    atan (Interval _) = error "not implemented"
    sinh (Interval _) = error "not implemented"
    cosh (Interval _) = error "not implemented"
    asinh (Interval _) = error "not implemented"
    acosh (Interval _) = error "not implemented"
    atanh (Interval _) = error "not implemented"
    log (Interval x) = Interval (log x)
-- instance Nullable Interval where
--     isnull = (== fromRational 0) . abs