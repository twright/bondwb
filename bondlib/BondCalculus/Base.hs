{-# LANGUAGE ExistentialQuantification, RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module BondCalculus.Base
  (Expression(..), Pretty(..), Nullable(..), DoubleExpression(..), BoolConc(..)) where

-- Pretty printing
class (Show a) => Pretty a where
  pretty :: a -> String
  pretty = show

class Expression a where
  simplify :: a -> a
  simplify = id

class (Num a, Floating a, Expression a) => DoubleExpression a where
  fromFloat :: Double -> a

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
