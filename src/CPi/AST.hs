{-# LANGUAGE DeriveGeneric #-}
module CPi.AST
  (
  Abstraction(..),
  Species(..),
  Name,
  Conc,
  Rate,
  Location,
  Prefix(..),
  Syntax(..),
  ProcessAlgebra(..),
  PrefixSpecies,
  RateLaw,
  RateLawFamily,
  Env,
  Pretty,
  Definition(..),
  concretify,
  pretty,
  maxLoc,
  prettyNames,
  prettyParens,
  colocate,
  instSpec,
  prefName,
  mkAbs,
  mkAbsBase,
  mkSum,
  mkPar,
  mkNew
  ) where

import qualified Data.List as L
import Data.Map (Map)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad
import GHC.Generics
import Data.Hashable
-- import Debug.Trace

trace a b = b

-- import qualified Data.Map as M

-- Core concepts
type Name = String
type Conc = Double
type Rate = Double

-- Rate laws
type RateLaw = [Conc] -> Rate
type RateLawFamily = [Double] -> RateLaw

-- Pretty printing
class (Show a) => Pretty a where
  pretty :: a -> String
  pretty = show

class (Pretty a) => Syntax a where
  relocate :: Location -> Location -> a -> a
  rename :: Name -> Name -> a -> a
  freeLocs :: a -> [Location]
  simplify :: a -> a
  simplify = id
  normalForm :: a -> a
  normalForm = simplify

class (Syntax a) => ProcessAlgebra a where
  (<|>) :: a -> a -> a
  -- Priority for adding parenthesis in pretty printing
  priority :: a -> Integer
  new :: [Location] -> a -> a
  boundLocs :: a -> [Location]

type Location = Integer

class (Syntax a) => Nameless a where
  maxLoc :: a -> Location

-- Parenthesisation
prettyParens :: (ProcessAlgebra a, ProcessAlgebra b) => a -> b -> String
prettyParens x x'
  | priority x < priority x' = pretty x
  | otherwise                = "(" ++ pretty x ++ ")"

data Prefix
  = Located !Name !Location
  | Unlocated !Name
  deriving (Eq, Ord, Show, Generic)

prefName :: Prefix -> Name
prefName (Located n _) = n
prefName (Unlocated n) = n

type PrefixSpecies = (Prefix, Abstraction)

data Definition = SpeciesDef
  { defArgs  :: [Name]
  , defLocs  :: [Location]
  , specBody :: Species }
  deriving (Eq, Ord, Show)

type Env = Map String Definition

type ExpId = Int

type NF = Species

data Species = Nil
             | Sum ExpId NF ![PrefixSpecies]
             | Par ExpId NF ![Species]
             | New ExpId NF ![Location] !Species
             | Def !String ![Name] ![Location]
               deriving (Generic)

instance Show Species where
  show Nil = "Nil"
  show (Par _ _ xs) = "mkPar " ++ show xs
  show (New _ _ locs x) = "mkNew " ++ show locs ++ " (" ++ show x ++ ")"
  show (Sum _ _ xs) = "mkSum " ++ show xs
  show (Def nm args locs) = "Def " ++ show nm ++ " " ++ show args ++ " "
                            ++ show locs

-- instance Hashable Species

instance Arbitrary Prefix where
  arbitrary = oneof [fmap Unlocated (elements ["x", "y", "z"]),
                     liftM2 Located (elements ["x", "y", "z"])
                                    (choose (0,3))]

instance Arbitrary Species where
  -- shrink = genericShrink
  arbitrary = sized genSpec
    where
      genSpec n
        | n > 0 = oneof [
          do
            m <- choose (0, n)
            specs <- vectorOf m $ genSpec (n `div` 4)
            return (mkPar specs)
        , do
            spec <- genSpec (n `div` 2)
            let mloc = maxLoc spec
            m <- choose (0, 3)
            locs <- vectorOf m (choose (mloc+1,mloc+5))
            return (new locs spec)
        , do
            m <- choose (0, n)
            let prefspec = do pref <- arbitrary
                              spec <- genSpec (n `div` 4)
                              let mloc = maxLoc spec
                              abst <- oneof [return (mkAbsBase spec),
                                             liftM2 mkAbs (choose (mloc+1,mloc+5)) (return spec)]
                              return (pref::Prefix, abst)
            prefspecs <- vectorOf m prefspec
            return (mkSum prefspecs)
        , return Nil
        , do
            name <- elements ["E", "S", "P"]
            margs <- choose (0, 3)
            args <- vectorOf margs (elements ["x", "y", "z"])
            mlocs <- choose (0, 3)
            locs <- vectorOf mlocs (choose (0, 10))
            return (Def name args locs)
        ]
        | otherwise = return Nil

instSpec :: Definition -> [Name] -> [Location] -> Species
instSpec (SpeciesDef (n:ns) ls body) (n':ns') ls' = rename n n'
  $ instSpec (SpeciesDef ns ls body) ns' ls'
instSpec (SpeciesDef ns (l:ls) body) ns' (l':ls') = relocate l l'
  $ instSpec (SpeciesDef ns ls body) ns' ls'
instSpec (SpeciesDef [] [] body) [] [] = body
instSpec spec@SpeciesDef{} ns ls = error $ "Applying definition with wrong number of args for " ++ show spec ++ ": args = " ++ show ns ++ ", locs = " ++ show ls

-- Abstractions are formed form species but allow abstracting over
-- variable name to allow for application (when a potential interaction)
-- is finalized.
data Abstraction
  = Abs ExpId !Location !Species
  | AbsBase ExpId !Species
  deriving (Generic)

instance Show Abstraction where
  show (Abs _ loc spec) = "Abs " ++ show loc ++ " (" ++ show spec ++ ")"
  show (AbsBase _ spec) = "AbsBase (" ++ show spec ++ ")"

mkAbs :: Location -> Species -> Abstraction
mkAbs loc spec = Abs expid loc spec
  where expid = 0 `hashWithSalt` loc `hashWithSalt` spec

mkAbsBase :: Species -> Abstraction
mkAbsBase spec = AbsBase expid spec
  where expid = 1 `hashWithSalt` spec

instance Hashable Species where
  hash Nil = 0
  hash (Def s nms locs) = 1 `hashWithSalt` s `hashWithSalt` nms
                          `hashWithSalt` locs
  hash (Sum expid _ _) = expid
  hash (Par expid _ _) = expid
  hash (New expid _ _ _) = expid

  hashWithSalt salt spec = hash (hash salt, hash spec)

mkSum :: [PrefixSpecies] -> Species
mkSum xs = res
  -- trace ("making sum " ++ pretty res ++ " from " ++ concatMap (\(x, y) -> pretty x ++ "->" ++ pretty y) xs) res
  where expid = 2 `hashWithSalt` map hash xs
        res = Sum expid nf xs
        nf = normalForm' res

mkPar :: [Species] -> Species
mkPar xs = res
  -- trace ("making par " ++ pretty res ++ " from " ++ show (map pretty xs)) res
  where expid = 3 `hashWithSalt` map hash xs
        res = Par expid nf xs
        nf = normalForm' res

mkNew :: [Location] -> Species -> Species
mkNew locs xs = trace ("making new from " ++ show locs ++ " and " ++ pretty xs) ans
  where expid = 4 `hashWithSalt` locs `hashWithSalt` xs
        -- nf = normalForm' res
        ans = let res = New expid nf locs xs
                  nf  = normalForm' res
              in res

expId :: Species -> Maybe Int
expId (Sum expid _ _) = Just expid
expId (Par expid _ _) = Just expid
expId (New expid _ _ _) = Just expid
expId x = Nothing

instance Hashable Abstraction where
  hash (Abs expid _ _) = expid
  hash (AbsBase expid _) = expid

  hashWithSalt salt abst = hash(hash salt, hash abst)


instance Eq Species where
  x == y = hash x == hash y

instance Ord Species where
  x `compare` y = hash x `compare` hash y

instance Eq Abstraction where
  x == y = hash x == hash y

instance Ord Abstraction where
  x `compare` y = hash x `compare` hash y

instance Hashable Prefix

instance Arbitrary Abstraction where
  arbitrary = oneof [ do
                        spec <- arbitrary
                        let mloc = maxLoc spec
                        l <- choose(mloc + 1, mloc + 5)
                        return (mkAbs l spec)
                    , fmap mkAbsBase arbitrary ]
  -- shrink = genericShrink

instance Syntax Prefix where
  relocate l l' p@(Located x m)
    | l == m = Located x l'
    | otherwise = p
  relocate _ _ p@(Unlocated _) = p

  rename x x' p@(Located y m)
    | x == y = Located x' m
    | otherwise = p
  rename x x' p@(Unlocated y)
    | x == y = Unlocated x'
    | otherwise = p

  freeLocs (Located _ l) = [l]
  freeLocs (Unlocated _) = []

instance Pretty Prefix where
  pretty (Located x l) = x ++ "@" ++ show l
  pretty (Unlocated x) = x

instance Nameless Prefix where
  maxLoc _ = 0


instance Syntax Species where
  relocate _ _ Nil = Nil
  relocate l l' (Sum _ _ xs)
    = mkSum [(relocate l l' pre, relocate l l' x)
          | (pre, x) <- xs]
  relocate l l' (Par _ _ xs) = mkPar $ map (relocate l l') xs
  -- should never be used to capture variables!
  relocate l l' (New _ _ ls spec) = mkNew ls $ if (l `elem` ls) || (l' `elem` ls) then spec
                                         else relocate l l' spec
  relocate l l' (Def name args locs) = Def name args [if loc == l
                                                      then l' else loc
                                                     | loc <- locs]

  rename _ _ Nil = Nil
  rename x x' (Sum _ _ prefspecs) = mkSum [(rename x x' pre, rename x x' spec)
                                    | (pre, spec) <- prefspecs]
  rename x x' (Par _ _ specs) = mkPar $ map (rename x x') specs
  rename x x' (New _ _ ls spec) = mkNew ls $ rename x x' spec
  rename x x' (Def name args locs) = Def name [if y == x then x'
                                               else y | y <- args] locs

  simplify = normalForm

  normalForm Nil = Nil
  normalForm d@Def{} = d
  normalForm (Sum _ nf _) = nf
  normalForm (Par _ nf _) = nf
  normalForm (New _ nf _ _) = nf

  freeLocs Nil = []
  freeLocs (Sum _ _ ss) = L.concat [freeLocs pref ++ freeLocs abst
    | (pref, abst) <- ss]
  freeLocs (Par _ _ ss) = foldr ((++) . freeLocs) [] ss
  freeLocs (New _ _ locs s) = filter (`notElem` locs) $ freeLocs s
  freeLocs (Def _ _ locs) = locs

normalForm' :: Species -> Species
normalForm' Nil = Nil
normalForm' d@Def{} = d
-- normalForm (Def expid _ args locs) = let nf = Def expid _ args locs
--                                      in nf
normalForm' (Sum _ _ []) = Nil
normalForm' sm@(Sum _ _ ss) = if nfss == ss then sm else mkSum nfss
  where nfss = L.sort [(pref, normalForm s) | (pref, s) <- ss]
normalForm' (Par _ _ [pr@Par{}]) = trace ("unwrapping" ++ pretty pr) $ normalForm pr
normalForm' pr@(Par _ _ ss) = trace ("normalizing pars " ++ pretty pr ++ "(" ++ show(expId pr) ++ ") to " ++ pretty res ++ "(" ++ show(expId res) ++ ")") res
  where res = case ss' of
          []  -> Nil
          [s] -> s
          _   -> if ss == ss' then trace ("bottom at pr = " ++ pretty pr ++ " given ss' = " ++ show (map pretty ss') ++ " ss = " ++ show (map pretty ss)) pr else res'
        res' = mkPar ss'
        flatten    = L.concatMap f
        f (Par _ _ ps) = ps
        f s        = [s]
        ss' = L.sort $ filter (/=Nil) $ flatten $ map normalForm ss
normalForm' (New _ _ [] s) = normalForm s
normalForm' (New _ _ locs1 (New _ _ locs2 s)) = normalForm $ mkNew (locs1 ++ locs2) s
normalForm' spec@(New _ _ locs s)
  | null locs' = trace ("Unwrapping to s' = " ++ pretty s') s'
  -- | null locs'' = trace ("Unwrapping to s'' = " ++ pretty s') s''
  | otherwise = s''''
    -- trace ("--- RETURN normal form of " ++ pretty spec)
    --             (if locs == locs'' && s == s'' then trace "returning unchanged" spec else trace "building new" s''')
  --  s'''
  -- if spec == s'''
  --   then trace ("settling at s''' (" ++ show(expId s''') ++") = " ++ pretty spec ++ " given spec (" ++ show(expId spec) ++ ") = " ++ pretty spec ++ "with s = " ++ pretty(s)) s'''
  --   else trace ("recursing on s''' (" ++ show(expId s''') ++ ") = " ++ pretty s''' ++ " given spec (" ++ show(expId spec) ++ ") = " ++ pretty spec) $ normalForm s'''
  where s'  = trace ("--- normal form of " ++ pretty spec) normalForm s
        locs' = L.sort $ L.nub $ L.intersect locs (freeLocs s')
        (locs'', s'') = reduceLocs locs' s'
        s''' = canonicallyReorderLocs locs'' s''
        -- nf'' = New expid'' nf'' locs'' s''
        -- expid'' = 4 `hashWithSalt` locs'' `hashWithSalt` s''
        s'''' = case splitFree locs'' s''' of
          Just nspec -> normalForm(nspec)
          Nothing -> if locs == locs'' && s == s''' then spec else normalForm(new locs'' s''')
        reduceLocs :: [Location] -> Species -> ([Location], Species)
        reduceLocs olocs m = (nLocs, if olocs == nLocs then m
                                     else normalForm m')
          where fLocs = filter (`notElem` olocs) (freeLocs m)
                bLocs = boundLocs m
                nextLocs = filter (\x -> (x `notElem` fLocs) && (x `notElem` bLocs)) [0..]
                locLen = length olocs
                nLocs = take locLen nextLocs
                safeLocs = filter (\x -> (x `notElem` nLocs) && (x `notElem` bLocs) && (x `notElem` freeLocs m)) [0..]
                middleLocs = take locLen safeLocs
                relocateAll ls ls' = foldl (.) id [relocate l l'
                                   | (l,l') <- zip ls ls']
                m' = relocateAll middleLocs nLocs
                   $ relocateAll olocs middleLocs m
        -- places the given location within a species within canonical order
        canonicallyReorderLocs :: [Location] -> Species -> Species
        canonicallyReorderLocs locs m = if length locs < 2 || m' == m
                                        then m else m'
          where fLocs = freeLocs m
                bLocs = boundLocs m
                relocateAll ls ls' = foldl (.) id [relocate l l'
                                   | (l,l') <- zip ls ls']
                safeLocs = filter (\x -> (x `notElem` bLocs) && (x `notElem` freeLocs m)) [0..]
                locLen = length locs
                middleLocs = take locLen safeLocs
                locPerms = L.permutations locs
                m's = [normalForm $ relocateAll middleLocs locs'
                      $ relocateAll locs middleLocs m | locs' <- locPerms]
                m' = trace("reordering based on " ++ show [(hash m', pretty m') | m' <- m's]) (head $ L.sortOn hash m's)


        splitFree :: [Location] -> Species -> Maybe Species
        splitFree ls pr@(Par _ _ ss) = if null outs then
                                       trace("settling inner at " ++ pretty plain) Nothing
                                    else trace ("split into " ++ pretty inexp ++ " and " ++ pretty outexp ++ " forming " ++ pretty res) $ Just $ normalForm res
          where res = mkPar [inexp, outexp]
                plain = trace ("making plain") (mkNew ls (normalForm pr))
                inexp = trace ("making in") (mkNew ls inpar)
                -- expid = 4 `hashWithSalt` ls `hashWithSalt` inpar
                outexp = trace ("making out") (normalForm(mkPar outs))
                inpar = trace ("making inpar") (normalForm(mkPar ins))
                        -- we cannot rely on normalForm to simplify trivial
                        -- pars as this may lead to infinite loops
                -- par [] = Nil
                -- par [w] = w
                -- par ws = mkPar (L.sort ws)
                ins    = filter (not . null . L.intersect ls . freeLocs) ss
                outs   = filter (null . L.intersect ls . freeLocs) ss
        splitFree ls x = Nothing
          -- where expid = 4 `hashWithSalt` ls `hashWithSalt` x
          --       nf    = New expid  ls x

instance ProcessAlgebra Species where
  Nil <|> Nil = Nil
  x <|> Nil = x
  Nil <|> y = y
  Par _ _ xs <|> Par _ _ ys = mkPar (xs ++ ys)
  x <|> Par _ _ ys = mkPar (x:ys)
  Par _ _ xs <|> y = mkPar (xs ++ [y])
  x <|> y = mkPar [x, y]

  new [] spec = spec
  new newlocs (New _ _ locs spec) = new (newlocs ++ locs) spec
  new newlocs spec
    | null newlocs' = spec
    | otherwise = mkNew newlocs' spec
    where newlocs' = L.nub $ L.intersect newlocs (freeLocs spec)

  boundLocs Nil = []
  boundLocs (Sum _ _ ss) = L.nub $ L.sort $ L.concat [boundLocs abst
    | (_, abst) <- ss]
  boundLocs (Par _ _ ss) = L.nub $ L.sort $ foldr ((++) . boundLocs) [] ss
  boundLocs (New _ _ locs s) = L.nub $ L.sort $ locs ++ boundLocs s
  boundLocs Def{} = []

  priority Nil = 5
  priority Def{} = 5
  priority (Sum _ _ ss)
    | length ss == 1 = 10
    | otherwise = 30
  priority (Par _ _ []) = 10
  priority (Par _ _ [x]) = succ $ priority x
  priority (Par _ _ _) = 30
  priority (New _ _ _ _) = 40

instance Nameless Species where
  maxLoc xs = if null locs then 0 else maximum locs
    where locs = boundLocs xs

instance Pretty Species where
  pretty Nil = "0"
  pretty (Sum _ _ x@((p,s):pss))
    | length x == 1
      = pretty p ++ "->" ++ pretty s
    | otherwise
      = pretty (mkSum [(p, s)]) ++ " + " ++ pretty (mkSum pss)
  pretty (Sum _ _ []) = "<Empty Sum>"
  pretty par@(Par _ _ specs) = prettyPar specs
    where prettyPar :: [Species] -> String
          prettyPar [] = "<Empty Par>"
          prettyPar [x] = prettyParens x par
          prettyPar (x:xs) = prettyParens x par ++ " | " ++ prettyPar xs
  pretty (New _ _ locs spec)
    = "new " ++ prettyNames locs ++ " in " ++ pretty spec
  pretty (Def name args locs)
    | null args && null locs = name
    | otherwise = name ++ "(" ++ L.intercalate "," args ++ ";"
                    ++ L.intercalate "," (map show locs) ++ ")"

instance Syntax Abstraction where
  relocate l l' (AbsBase _ spec) = mkAbsBase $ relocate l l' spec
  -- should never be used to capture variables!
  relocate l l' (Abs _ m abst) = mkAbs m $ if l == m || l' == m then abst
                                       else relocate l l' abst

  rename x x' (AbsBase _ spec) = mkAbsBase $ rename x x' spec
  rename x x' (Abs _ m abst) = mkAbs m $ rename x x' abst

  freeLocs (AbsBase _ spec) = freeLocs spec
  freeLocs (Abs _ m abst) = filter (/=m) $ freeLocs abst

  simplify = normalForm

  normalForm (AbsBase _ x) = mkAbsBase (normalForm x)
  normalForm (Abs _ l x) = if l `notElem` freeLocs x' then mkAbsBase x'
                 else mkAbs l' (if l == l' then x'
                              else normalForm $ relocate l l' x')
    where x' = normalForm x
          bLocs = boundLocs x'
          fLocs = filter (/=l) $ freeLocs x'
          nextLocs = filter (\y -> (y `notElem` fLocs) && (y `notElem` bLocs)) [0..]
          l' = head nextLocs

instance ProcessAlgebra Abstraction where
  (<|>) = colocate

  new locs (AbsBase _ spec) = mkAbsBase $ new locs spec
  new locs (Abs _ m spec) = mkAbs m $ new locs spec

  priority (AbsBase _ spec) = priority spec
  priority (Abs _ _ _) = 11

  boundLocs (Abs _ l spec) = L.nub $ L.sort $ l:boundLocs spec
  boundLocs (AbsBase _ spec) = L.nub $ L.sort $ boundLocs spec

instance Nameless Abstraction where
  maxLoc xs = if null locs then 0 else maximum locs
    where locs = boundLocs xs

instance Pretty Abstraction where
  pretty x@(Abs _ l spec)
    = "("++ show l ++ ")" ++ prettyParens spec x
  pretty (AbsBase _ spec)
    = pretty spec

colocate :: Abstraction -> Abstraction -> Abstraction
colocate (Abs _ l x) (Abs _ m y) = mkAbs o (x' <|> y')
  where o = max l m
        x' = relocate l o x
        y' = relocate m o y
colocate (Abs _ l x) (AbsBase _ y) = mkAbs o (x' <|> y)
  where o = maximum (l:map (+1) (freeLocs y ++ boundLocs y ++ filter (/=l) (freeLocs x)))
        x' = relocate l o x
colocate (AbsBase _ x) (Abs _ l y) = mkAbs o (x <|> y')
  where o = maximum (l:map (+1) (freeLocs x ++ boundLocs x ++ filter (/=l) (freeLocs y)))
        y' = relocate l o y
colocate (AbsBase _ x) (AbsBase _ y) = mkAbsBase (x <|> y)

concretify :: Abstraction -> Species
concretify (AbsBase _ spec) = spec
concretify (Abs _ l spec) = new [l] spec

-- Pretty print a list of Names.
prettyNames :: (Show a) => [a] -> String
prettyNames ns = L.intercalate "," (map show ns)
