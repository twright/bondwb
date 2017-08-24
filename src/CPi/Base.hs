module CPi.Base
  (Expression(..), Pretty(..), Nullable(..), DoubleExpression(..)) where

-- Pretty printing
class (Show a) => Pretty a where
  pretty :: a -> String
  pretty = show

class Expression a where
  simplify :: a -> a
  simplify = id

class (Expression a) => DoubleExpression a where
  fromFloat :: Double -> a

instance Expression Double where
instance Expression Integer where
instance DoubleExpression Double where
  fromFloat = id

class Nullable a where
  isnull :: a -> Bool

instance Nullable Double where
  isnull = (<1e-16)
