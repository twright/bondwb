{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module CPi.Transitions
  (Transition(..), MTS, TransitionSemantics(..), union, potentialTrans, finalTrans,
    (--:), (-->), (==:), (==>)) where

import CPi.AST
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M

data TransitionStatus = Final | Potential
                        deriving (Show, Eq, Ord)

data Transition = Trans TransitionStatus Species [Prefix] Abstraction
                -- | TransF Species [Prefix] Species
                deriving (Show, Eq, Ord)

-- Implement fancy arrows for denoting transitions in multisets
-- x --:locs--> y == Trans  x locs y
-- x ==:locs==> y == TransF x locs y
(--:) :: Species -> [Prefix] -> (Abstraction -> Transition)
spec --: xs = Trans Potential spec xs 
(-->) :: (Abstraction -> Transition) -> Abstraction -> Transition
f --> abst = f abst
(==:) :: Species -> [Prefix] -> (Abstraction -> Transition)
spec ==: xs = Trans Final spec xs 
(==>) :: (Abstraction -> Transition) -> Abstraction -> Transition
f ==> abst = f abst
infixr 2 --:, ==:
infixr 1 -->, ==>

instance Pretty Transition where
  pretty (Trans Potential x prefs y) = pretty x
    ++ " --{" ++ L.intercalate "," (map pretty prefs) ++ "}--> "
    ++ pretty y
  pretty (Trans Final x prefs y) = pretty x
    ++ " =={" ++ L.intercalate "," (map pretty prefs) ++ "}==> "
    ++ pretty y

delocate :: Prefix -> Prefix
delocate (Located x _) = Unlocated x
delocate u@Unlocated{} = u

finalize :: Transition -> Transition
-- finalize t@Trans Final _ _ = t
finalize (Trans status x prefs y) = Trans status x (map delocate prefs) (AbsBase $ concretify y) 

instance Syntax Transition where
  simplify (Trans status x prefs y)  = Trans status (simplify x) (L.sort prefs) (simplify y)
  -- simplify (TransF x prefs y) = simplify x ==:L.sort prefs==> simplify y

  relocate l l' (Trans Potential x prefs y)  = relocate l l' x
                                     --:map (relocate l l') prefs-->
                                     relocate l l' y
  relocate l l' (Trans Final x prefs y) = relocate l l' x
                                     ==:map (relocate l l') prefs==>
                                     relocate l l' y

  freeLocs (Trans _ x prefs y)  = foldr ((++).freeLocs) [] prefs
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
potentialTrans ts = [ t | t@(Trans Potential _ _ _) <- ts ]

finalTrans :: MTS -> MTS
finalTrans ts = [ t | t@(Trans Final _ _ _) <- ts ]

-- instance {-# OVERLAPPING #-} Eq MTS where
--   xs == ys = (simplify xs) (L.(==)) (simplify ys)
    -- where MTS xs = simplify x 
    --       MTS ys = simplify y


class (Pretty a) => TransitionSemantics a where
  trans :: a -> Env -> MTS
  transF :: a -> Env -> MTS
  transF x env = fmap finalize $ trans x env

instance TransitionSemantics Species where
  trans Nil _ = []
  trans (Par (t:ts)) env = [
      -- new reactions
      x <|> y --:(ls++ms)--> (x' <|> y')
      | Trans Potential x ls x' <- hd, Trans Potential y ms y' <- tl
    ] ++ [
      -- combining reactions on the left
      Trans status (x <|> sTail) ls (x' <|> AbsBase sTail)
      | Trans status x ls x' <- hd
    ] ++ [
      -- combining potential reaction on the right
      Trans status (t <|> y) ms (AbsBase t <|> y')
      | Trans status y ms y' <- tl
    ]
    where sTail = Par ts
          hd = potentialTrans $ trans t env
          tl = potentialTrans $ trans sTail env
  trans (Par []) _ = []
  trans (New newlocs spec) env = (++)
    [if null (newlocs `L.intersect` foldr ((++).freeLocs) [] locs)
        then new newlocs x --:locs--> new newlocs y
        else new newlocs x ==:map delocate locs==> AbsBase (new newlocs (concretify y))
      | Trans Potential x locs y <- specMTS] 
    [new newlocs x ==:locs==> new newlocs y | Trans Final x locs y <- specMTS]
    where specMTS = trans spec env
  trans x@(Sum prefspecs) env = [x --:[pref]--> y | (pref, y) <- prefspecs]
  trans d@(Def name args locs) env = case M.lookup name env of
    Just specdef ->
      [Trans status d locs y | Trans status _ locs y <- specMTS]
      where specMTS = trans (instSpec specdef args locs) env
    Nothing      -> error $ "Species " ++ name ++ " not defined"

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