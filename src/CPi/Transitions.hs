{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module CPi.Transitions
  (Transition(..), MTS, TransitionSemantics(..), TransitionSemanticsFiltered(..), TransitionStatus(..), union, potentialTrans, finalTrans, PrefixFilter,
    (--:), (-->), (==:), (==>)) where

import CPi.AST
import qualified Data.List as L
-- import Data.Map (Map)
import qualified Data.Map as M
import Data.Function(fix)

data TransitionStatus = Final | Potential
                        deriving (Show, Eq, Ord)
-- deriveMemoizable ''TransitionStatus

data Transition = Trans TransitionStatus Species [Prefix] Abstraction
                  deriving (Show, Eq, Ord)
-- deriveMemoizable ''Transition

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
finalize (Trans status x prefs y) = Trans status x (map delocate prefs) (mkAbsBase $ concretify y)

instance Syntax Transition where
  simplify (Trans status x prefs y)  = Trans status (simplify x) (L.sort prefs) (simplify y)

  relocate l l' (Trans status x prefs y) = Trans status (relocate l l' x)
                                           (map (relocate l l') prefs)
                                           (relocate l l' y)

  rename n n' (Trans status x prefs y) = Trans status (rename n n' x)
                                           (map (rename n n') prefs)
                                           (rename n n' y)

  freeLocs (Trans _ x prefs y)  = foldr ((++).freeLocs) [] prefs
                                ++ freeLocs x ++ freeLocs y

instance Pretty MTS where
  pretty xs = "{| "
    ++ L.intercalate ",\n   " (L.sort $ map pretty xs)
    ++ " |}"

instance Syntax MTS where
  simplify = L.sort . map simplify

  relocate l l' = fmap (relocate l l')

  rename n n' = fmap (rename n n')

  freeLocs = concatMap freeLocs

type MTS = [Transition]

union :: MTS -> MTS -> MTS
union = (++)

potentialTrans :: MTS -> MTS
potentialTrans ts = [ t | t@(Trans Potential _ _ _) <- ts ]

finalTrans :: MTS -> MTS
finalTrans ts = [ t | t@(Trans Final _ _ _) <- ts ]

class (Pretty a) => TransitionSemantics a where
  trans :: a -> Env -> MTS
  transF :: a -> Env -> MTS
  transF x env = finalize <$> trans x env

type PrefixFilter = TransitionStatus -> [String] -> Bool

class (Pretty a) => TransitionSemanticsFiltered a where
  transFiltered ::  PrefixFilter -> Env -> a -> MTS

filterTransitions :: PrefixFilter -> MTS -> MTS
filterTransitions validPref = filter validTrans
  where validTrans (Trans s _ ls _) = validPref s (L.sort $ map prefName ls)

instance TransitionSemanticsFiltered Species where
  transFiltered validTrans env = trF `seq` fix trF
    where
      trF :: (Species -> MTS) -> Species -> MTS
      trF s s' = filterTransitions validTrans $ tr s s'
      tr :: (Species -> MTS) -> Species -> MTS
      tr trans Nil = []
      tr trans (Par _ _ (t:ts)) = [
          -- new reactions
          x <|> y --:(ls++ms)--> (x' <|> y')
          | Trans Potential x ls x' <- hd, Trans Potential y ms y' <- tl
        ] ++ [
          -- combining reactions on the left
          Trans status (x <|> sTail) ls (x' <|> mkAbsBase sTail)
          | Trans status x ls x' <- hd
        ] ++ [
          -- combining potential reaction on the right
          Trans status (t <|> y) ms (mkAbsBase t <|> y')
          | Trans status y ms y' <- tl
        ]
        where sTail = mkPar ts
              hd = trans t
              tl = trans sTail
      tr _ (Par _ _ []) = []
      tr trans (New _ _ newlocs spec) = (++)
        [if null (newlocs `L.intersect` foldr ((++).freeLocs) [] locs)
            then new newlocs x --:locs--> new newlocs y
            else new newlocs x ==:map delocate locs==> mkAbsBase (new newlocs (concretify y))
          | Trans Potential x locs y <- specMTS]
        [new newlocs x ==:locs==> new newlocs y | Trans Final x locs y <- specMTS]
        where specMTS = trans spec
      tr _ x@(Sum _ _ prefspecs) = [x --:[pref]--> y | (pref, y) <- prefspecs]
      tr trans d@(Def name args locs) = case M.lookup name env of
        Just specdef ->
          [Trans status d locs' y | Trans status _ locs' y <- specMTS]
          where specMTS = trans (instSpec specdef args locs)
        Nothing      -> error $ "Species " ++ name ++ " not defined"

instance TransitionSemantics Species where
  trans Nil _ = []
  trans (Par _ _ (t:ts)) env = [
      -- new reactions
      x <|> y --:(ls++ms)--> (x' <|> y')
      | Trans Potential x ls x' <- hd, Trans Potential y ms y' <- tl
    ] ++ [
      -- combining reactions on the left
      Trans status (x <|> sTail) ls (x' <|> mkAbsBase sTail)
      | Trans status x ls x' <- hd
    ] ++ [
      -- combining potential reaction on the right
      Trans status (t <|> y) ms (mkAbsBase t <|> y')
      | Trans status y ms y' <- tl
    ]
    where sTail = mkPar ts
          hd = potentialTrans $ trans t env
          tl = potentialTrans $ trans sTail env
  trans (Par _ _ []) _ = []
  trans (New _ _ newlocs spec) env = (++)
    [if null (newlocs `L.intersect` foldr ((++).freeLocs) [] locs)
        then new newlocs x --:locs--> new newlocs y
        else new newlocs x ==:map delocate locs==> mkAbsBase (new newlocs (concretify y))
      | Trans Potential x locs y <- specMTS]
    [new newlocs x ==:locs==> new newlocs y | Trans Final x locs y <- specMTS]
    where specMTS = trans spec env
  trans x@(Sum _ _ prefspecs) _ = [x --:[pref]--> y | (pref, y) <- prefspecs]
  trans d@(Def name args locs) env = case M.lookup name env of
    Just specdef ->
      [Trans status d locs' y | Trans status _ locs' y <- specMTS]
      where specMTS = trans (instSpec specdef args locs) env
    Nothing      -> error $ "Species " ++ name ++ " not defined"
