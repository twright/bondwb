{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module CPi.Transitions
  (Transition(..), MTS, TransitionSemantics(..), union, potentialTrans, finalTrans,
    (--:), (-->), (==:), (==>)) where

import CPi.AST
import qualified Data.List as L

data Transition = Trans Species [Prefix] Abstraction
                | TransF Species [Prefix] Species
                deriving (Show, Eq, Ord)

-- Implement fancy arrows for denoting transitions in multisets
-- x --:locs--> y == Trans  x locs y
-- x ==:locs==> y == TransF x locs y
(--:) :: Species -> [Prefix] -> (Abstraction -> Transition)
spec --: xs = Trans spec xs 
(-->) :: (Abstraction -> Transition) -> Abstraction -> Transition
f --> abst = f abst
(==:) :: Species -> [Prefix] -> (Species -> Transition)
spec ==: xs = TransF spec xs 
(==>) :: (Species -> Transition) -> Species -> Transition
f ==> abst = f abst
infixr 2 --:, ==:
infixr 1 -->, ==>

instance Pretty Transition where
  pretty (Trans x prefs y) = pretty x
    ++ " --{" ++ L.intercalate "," (map pretty prefs) ++ "}--> "
    ++ pretty y
  pretty (TransF x prefs y) = pretty x
    ++ " =={" ++ L.intercalate "," (map pretty prefs) ++ "}==> "
    ++ pretty y

delocate :: Prefix -> Prefix
delocate (Located x _) = Unlocated x
delocate u@Unlocated{} = u

finalize :: Transition -> Transition
finalize t@TransF{} = t
finalize (Trans x prefs y) = TransF x (map delocate prefs) (concretify y) 

instance Syntax Transition where
  simplify (Trans x prefs y)  = simplify x --:L.sort prefs--> simplify y
  simplify (TransF x prefs y) = simplify x ==:L.sort prefs==> simplify y

  relocate l l' (Trans x prefs y)  = relocate l l' x
                                     --:map (relocate l l') prefs-->
                                     relocate l l' y
  relocate l l' (TransF x prefs y) = relocate l l' x
                                     ==:map (relocate l l') prefs==>
                                     relocate l l' y

  freeLocs (Trans x prefs y)  = foldr ((++).freeLocs) [] prefs
                                ++ freeLocs x ++ freeLocs y
  freeLocs (TransF x prefs y) = foldr ((++).freeLocs) [] prefs
                                ++ freeLocs x ++ freeLocs y

instance Pretty MTS where
  pretty xs = "{| "
    ++ L.intercalate ",\n   " (L.sort $ map pretty xs)
    ++ " |}" 

instance Syntax MTS where
  simplify = L.sort . map simplify

  relocate l l' = fmap (relocate l l')

  freeLocs = concatMap freeLocs


type MTS = [Transition]

union :: MTS -> MTS -> MTS
union = (++)

potentialTrans :: MTS -> MTS
potentialTrans ts = [ t | t@Trans{} <- ts ]

finalTrans :: MTS -> MTS
finalTrans ts = [ t | t@TransF{} <- ts ]

-- instance {-# OVERLAPPING #-} Eq MTS where
--   xs == ys = (simplify xs) (L.(==)) (simplify ys)
    -- where MTS xs = simplify x 
    --       MTS ys = simplify y


class (Pretty a) => TransitionSemantics a where
  trans :: a -> MTS
  transF :: a -> MTS
  transF = fmap finalize . trans

instance TransitionSemantics Species where
  trans Nil = []
  trans (Par (t:ts)) = [
      -- new reactions
      x <|> y --:(ls++ms)--> (x' <|> y')
      | Trans x ls x' <- hd, Trans y ms y' <- tl
    ] ++ [
      -- combining potential reaction on the left
      x <|> sTail --:ls--> x' <|> AbsBase sTail
      | Trans x ls x' <- hd
    ] ++ [
      -- combining potential reaction on the right
      t <|> y --:ms--> AbsBase t <|> y'
      | Trans y ms y' <- tl
    ] ++ [
      -- combining final reaction on the left
      x <|> sTail ==:ls==> x' <|> sTail
      | TransF x ls x' <- hd
    ] ++ [
      -- combining final reaction on the right
      t <|> y ==:ms==> t <|> y'
      | TransF y ms y' <- tl
    ]
    where sTail = Par ts
          hd = potentialTrans $ trans t
          tl = potentialTrans $ trans sTail
  trans (Par []) = []
  trans (New newlocs spec) = (++)
    [if null (newlocs `L.intersect` foldr ((++).freeLocs) [] locs)
        then new newlocs x --:locs--> new newlocs y
        else new newlocs x ==:map delocate locs==> new newlocs (concretify y)
      | Trans x locs y <- specMTS] 
    [new newlocs x ==:locs==> new newlocs y | TransF x locs y <- specMTS]
    where specMTS = trans spec
  trans x@(Sum prefspecs) = [Trans x [pref] y | (pref, y) <- prefspecs]

-- instance TransitionSemantics Abstraction where
--   trans (AbsBase spec) = trans spec
--   trans (AbsPar abss) = foldl union (MTS []) (map trans abss) 
--   trans (AbsNew newlocs abstr) = MTS $ (++)
--     [Trans (AbsNew newlocs x) locs (AbsNew newlocs y)
--       | Trans x locs y <- absMTS] 
--     [TransF (AbsNew newlocs y) locs (AbsNew newlocs y)
--       | TransF x locs y <- absMTS]
--     where
--       absMTS = MTS abstr
--   trans (Abs _ _) = MTS[]
  -- trans AbsPar  