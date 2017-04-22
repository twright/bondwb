{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module CPi.Transitions
  (Transition(..), MTS, TransitionSemantics(..),
   TransitionSemanticsFiltered(..), PrefixFilter, (--:), (-->)) where

import CPi.AST
import qualified Data.List as L
import qualified Data.Map as M
import Data.Function(fix)
import Data.Maybe


data Transition = Trans Species [Prefix] Abstraction
                  deriving (Show, Eq, Ord)

-- Implement fancy arrows for denoting transitions in multisets
-- x --:locs--> y == Trans  x locs y
(--:) :: Species -> [Prefix] -> (Abstraction -> Transition)
spec --: xs = Trans spec xs
(-->) :: (Abstraction -> Transition) -> Abstraction -> Transition
f --> abst = f abst
infixr 2 --:
infixr 1 -->

instance Pretty Transition where
  pretty (Trans x prefs y) = pretty x
    ++ " --{" ++ L.intercalate "," (map pretty prefs) ++ "}--> "
    ++ pretty y

delocate :: Prefix -> Prefix
delocate (Located x _) = Unlocated x
delocate u@Unlocated{} = u

finalize :: Transition -> Transition
finalize (Trans x prefs y) = Trans x (map delocate prefs) (mkAbsBase $ concretify y)

instance Syntax Transition where
  simplify (Trans x prefs y)  = simplify x --:L.sort prefs--> simplify y

  relocate l l' (Trans x prefs y) = relocate l l' x
                                    --:map (relocate l l') prefs-->
                                    relocate l l' y

  relocateAll ls ls' (Trans x prefs y) = relocateAll ls ls' x
                                         --:map (relocateAll ls ls') prefs-->
                                         relocateAll ls ls' y

  rename n n' (Trans x prefs y) = rename n n' x
                                  --:map (rename n n') prefs-->
                                  rename n n' y

  freeLocs (Trans x prefs y)  = foldr ((++).freeLocs) [] prefs
                                ++ freeLocs x ++ freeLocs y

instance Pretty MTS where
  pretty xs = "{| "
    ++ L.intercalate ",\n   " (L.sort $ map pretty xs)
    ++ " |}"

instance Syntax MTS where
  simplify = L.sort . map simplify

  relocate l l' = fmap (relocate l l')

  relocateAll ls ls' = fmap (relocateAll ls ls')

  rename n n' = fmap (rename n n')

  freeLocs = concatMap freeLocs

type MTS = [Transition]

class (Pretty a) => TransitionSemantics a where
  trans :: Env -> a -> MTS
  transF :: Env -> a -> MTS
  transF env x = finalize <$> trans env x

type PrefixFilter = [String] -> Bool

class (Pretty a) => TransitionSemanticsFiltered a where
  transFiltered ::  PrefixFilter -> Env -> a -> MTS

filterTransitions :: PrefixFilter -> MTS -> MTS
filterTransitions validPref = filter validTrans
  where validTrans (Trans _ ls _) = validPref (L.sort $ map prefName ls)

instance TransitionSemanticsFiltered Species where
  transFiltered validTrans env = trF `seq` fix trF
    where
      trF :: (Species -> MTS) -> Species -> MTS
      trF s s' = filterTransitions validTrans $ tr s s'
      tr :: (Species -> MTS) -> Species -> MTS
      tr _ Nil = []
      tr tr' (Par _ _ (t:ts)) = [
          -- new reactions
          x <|> y --:(ls++ms)--> x' <|> y'
          | Trans x ls x' <- hd, Trans y ms y' <- tl,
            isJust (location ls), isJust(location ms),
            location ls == location ms
        ] ++ [
          -- combining reactions on the left
          x <|> sTail --:ls--> x' <|> mkAbsBase sTail
          | Trans x ls x' <- hd
        ] ++ [
          -- combining reaction on the right
          t <|> y --:ms--> mkAbsBase t <|> y'
          | Trans y ms y' <- tl
        ]
        where sTail = mkPar ts
              hd = tr' t
              tl = tr' sTail
      tr _ (Par _ _ []) = []
      tr tr' (New _ _ newlocs spec) =
        [if null (newlocs `L.intersect` foldr ((++).freeLocs) [] locs)
            then new newlocs x --:locs--> new newlocs y
            else new newlocs x --:map delocate locs--> mkAbsBase (new newlocs (concretify y))
          | Trans x locs y <- specMTS]
        where specMTS = tr' spec
      tr _ x@(Sum _ _ prefspecs) = [x --:[pref]--> y | (pref, y) <- prefspecs]
      tr tr' d@(Def name args locs) = case M.lookup name env of
        Just specdef ->
          [Trans d locs' y | Trans _ locs' y <- specMTS]
          where specMTS = tr' (instSpec specdef args locs)
        Nothing      -> error $ "Species " ++ name ++ " not defined"
      location :: [Prefix] -> Maybe Location
      location prefs = case map prefLoc prefs of
                         [Just loc] -> Just loc
                         _          -> Nothing

instance TransitionSemantics Species where
  trans = transFiltered (const True)
