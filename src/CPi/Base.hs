module CPi.Base
  (Expression(..), Pretty(..)) where

-- Pretty printing
class (Show a) => Pretty a where
  pretty :: a -> String
  pretty = show

class Expression a where
  simplify :: a -> a
  simplify = id

instance Expression Double where
instance Expression Integer where
