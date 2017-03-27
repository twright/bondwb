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
  instSpec
  ) where

import qualified Data.List as L
import Data.Map (Map)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad
import GHC.Generics
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
  = Located Name Location
  | Unlocated Name
  deriving (Eq, Ord, Show)

type PrefixSpecies = (Prefix, Abstraction)

data Definition = SpeciesDef
  { defArgs  :: [Name]
  , defLocs  :: [Location]
  , specBody :: Species }
  deriving (Eq, Ord, Show)

type Env = Map String Definition

data Species = Nil
             | Sum [PrefixSpecies]
             | Par [Species]
             | New [Location] Species
             | Def String [Name] [Location]
               deriving (Eq, Ord, Show, Generic)

instance Arbitrary Prefix where
  arbitrary = oneof [fmap Unlocated (elements ["x", "y", "z"]),
                     liftM2 Located (elements ["x", "y", "z"])
                                    (choose (0,3))]

instance Arbitrary Species where
  shrink = genericShrink
  arbitrary = sized genSpec
    where
      genSpec n
        | n > 0 = oneof [
          do
            m <- choose (0, n)
            specs <- vectorOf m $ genSpec (n `div` 4)
            return (Par specs)
        , do
            spec <- genSpec (n `div` 2)
            let mloc = maxLoc spec
            m <- choose (0, 3)
            locs <- vectorOf m (choose (mloc+1,mloc+5))
            return (New locs spec)
        , do
            m <- choose (0, n)
            let prefspec = do pref <- arbitrary
                              spec <- genSpec (n `div` 4)
                              let mloc = maxLoc spec
                              abst <- oneof [return (AbsBase spec),
                                             liftM2 Abs (choose (mloc+1,mloc+5)) (return spec)]
                              return (pref::Prefix, abst)
            prefspecs <- vectorOf m prefspec
            return (Sum prefspecs)
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
instSpec (SpeciesDef (n:ns) (l:ls) body) (n':ns') (l':ls') = rename n n' $ relocate l l'
  $ instSpec (SpeciesDef ns ls body) ns' ls'
instSpec (SpeciesDef [] [] body) [] [] = body
instSpec SpeciesDef{} _ _ = error "Applying definition with wrong number of args"


-- Abstractions are formed form species but allow abstracting over
-- variable name to allow for application (when a potential interaction)
-- is finalized.
data Abstraction
  = Abs Location Species
  | AbsBase Species
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary Abstraction where
  arbitrary = oneof [ do
                        spec <- arbitrary
                        let mloc = maxLoc spec
                        l <- choose(mloc + 1, mloc + 5)
                        return (Abs l spec)
                    , fmap AbsBase arbitrary ]
  shrink = genericShrink

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
  relocate l l' (Sum xs)
    = Sum [(relocate l l' pre, relocate l l' x)
          | (pre, x) <- xs]
  relocate l l' (Par xs) = Par $ map (relocate l l') xs
  -- should never be used to capture variables!
  relocate l l' (New ls spec) = New ls $ if (l `elem` ls) || (l' `elem` ls) then spec
                                         else relocate l l' spec
  relocate l l' (Def name args locs) = Def name args [if loc == l
                                                      then l' else loc
                                                     | loc <- locs]

  rename _ _ Nil = Nil
  rename x x' (Sum prefspecs) = Sum [(rename x x' pre, rename x x' spec)
                                    | (pre, spec) <- prefspecs]
  rename x x' (Par specs) = Par $ map (rename x x') specs
  rename x x' (New ls spec) = New ls $ rename x x' spec
  rename x x' (Def name args locs) = Def name [if y == x then x'
                                               else y | y <- args] locs

  simplify = normalForm

  normalForm spec
    | res == spec = res
    | otherwise   = normalForm res
    where res = nf spec
          nf Nil = Nil
          nf d@Def{} = d
          nf (Sum []) = Nil
          nf (Sum ss) = Sum $ L.sort [(pref, normalForm s) | (pref, s) <- ss]
          nf (Par []) = Nil
          nf (Par [s]) = normalForm s
          nf (Par ss) = Par $ L.sort $ filter (/=Nil) $ flatten $ map normalForm ss
            where flatten    = L.concatMap f
                  f (Par ps) = ps
                  f s        = [s]
          nf (New [] s) = s
          nf (New locs1 (New locs2 s)) = New (locs1 ++ locs2) $ normalForm s
          nf (New locs s)
            | null locs' = s'
            | otherwise = s'''
            where s'  = normalForm s
                  locs' = L.sort $ L.nub $ L.intersect locs $ freeLocs s'
                  s'' = case highers of
                    [] -> s'
                    (h:_) -> relocate h nextLoc s'
                  locs'' = case highers of
                    [] -> locs'
                    h:_ -> [if l == h then nextLoc else l | l <- locs']
                  s''' = case s'' of
                    Par ss -> inexp <|> outexp
                      where ins    = filter (not . null . L.intersect locs'' . freeLocs) ss
                            outs   = filter (null . L.intersect locs'' . freeLocs) ss
                            inexp  = New locs'' (Par ins)
                            outexp = case outs of
                              []  -> Nil
                              [w] -> w
                              ws  -> Par ws
                    _ -> new locs'' s''
                  highers = filter (> nextLoc) locs'
                  nextLocs = map (+1) (boundLocs s' ++ filter (`notElem` locs') (freeLocs s'))
                  nextLoc = if null nextLocs then 0 else maximum nextLocs

  freeLocs Nil = []
  freeLocs (Sum ss) = L.concat [freeLocs pref ++ freeLocs abst
    | (pref, abst) <- ss]
  freeLocs (Par ss) = foldr ((++) . freeLocs) [] ss
  freeLocs (New locs s) = filter (`notElem` locs) $ freeLocs s
  freeLocs (Def _ _ locs) = locs

instance ProcessAlgebra Species where
  Nil <|> Nil = Nil
  x <|> Nil = x
  Nil <|> y = y
  Par xs <|> Par ys = Par (xs ++ ys)
  x <|> Par ys = Par (x:ys)
  Par xs <|> y = Par (xs ++ [y])
  x <|> y = Par [x, y]

  new [] spec = spec
  new newlocs (New locs spec) = new (newlocs ++ locs) spec
  new newlocs spec
    | null newlocs' = spec
    | otherwise = New newlocs' spec
    where newlocs' = L.nub $ L.intersect newlocs (freeLocs spec)

  boundLocs Nil = []
  boundLocs (Sum ss) = L.nub $ L.sort $ L.concat [boundLocs abst
    | (_, abst) <- ss]
  boundLocs (Par ss) = L.nub $ L.sort $ foldr ((++) . boundLocs) [] ss
  boundLocs (New locs s) = L.nub $ L.sort $ locs ++ boundLocs s
  boundLocs Def{} = []

  priority Nil = 5
  priority Def{} = 5
  priority (Sum ss)
    | length ss == 1 = 10
    | otherwise = 30
  priority (Par []) = 10
  priority (Par [x]) = succ $ priority x
  priority (Par _) = 30
  priority (New _ _) = 40

instance Nameless Species where
  maxLoc xs = if null locs then 0 else maximum locs
    where locs = boundLocs xs

instance Pretty Species where
  pretty Nil = "0"
  pretty (Sum x@((p,s):pss))
    | length x == 1
      = pretty p ++ "->" ++ pretty s
    | otherwise
      = pretty (Sum [(p, s)]) ++ " + " ++ pretty (Sum pss)
  pretty (Sum []) = "<Empty Sum>"
  pretty par@(Par specs) = prettyPar specs
    where prettyPar :: [Species] -> String
          prettyPar [] = "<Empty Par>"
          prettyPar [x] = prettyParens x par
          prettyPar (x:xs) = prettyParens x par ++ " | " ++ prettyPar xs
  pretty (New locs spec)
    = "new " ++ prettyNames locs ++ " in " ++ pretty spec
  pretty (Def name args locs)
    | null args && null locs = name
    | otherwise = name ++ "(" ++ L.intercalate "," args ++ ";"
                    ++ L.intercalate "," (map show locs) ++ ")"

instance Syntax Abstraction where
  relocate l l' (AbsBase spec) = AbsBase $ relocate l l' spec
  -- should never be used to capture variables!
  relocate l l' (Abs m abst) = Abs m $ if l == m || l' == m then abst
                                       else relocate l l' abst

  rename x x' (AbsBase spec) = AbsBase $ rename x x' spec
  rename x x' (Abs m abst) = Abs m $ rename x x' abst

  freeLocs (AbsBase spec) = freeLocs spec
  freeLocs (Abs m abst) = filter (/=m) $ freeLocs abst

  simplify = normalForm

  normalForm abst
    | abst == res = res
    | otherwise = normalForm res
    where res = nf abst
          nf (AbsBase x) = AbsBase (normalForm x)
          nf (Abs l x) = if l `notElem` freeLocs x' then AbsBase x'
                         else Abs l' (if l == l' then x'
                                      else normalForm $ relocate l l' x')
            where x' = normalForm x
                  l' = if null (boundLocs x')
                       then 0
                       else maxLoc x + 1

instance ProcessAlgebra Abstraction where
  (<|>) = colocate

  new locs (AbsBase spec) = AbsBase $ new locs spec
  new locs (Abs m spec) = Abs m $ new locs spec

  priority (AbsBase spec) = priority spec
  priority (Abs _ _) = 11

  boundLocs (Abs l spec) = L.nub $ L.sort $ l:boundLocs spec
  boundLocs (AbsBase spec) = L.nub $ L.sort $ boundLocs spec

instance Nameless Abstraction where
  maxLoc xs = if null locs then 0 else maximum locs
    where locs = boundLocs xs

instance Pretty Abstraction where
  pretty x@(Abs l spec)
    = "("++ show l ++ ")" ++ prettyParens spec x
  pretty (AbsBase spec)
    = pretty spec

colocate :: Abstraction -> Abstraction -> Abstraction
colocate (Abs l x) (Abs m y) = Abs o (x' <|> y')
  where o = max l m
        x' = relocate l o x
        y' = relocate m o y
colocate (Abs l x) (AbsBase y) = Abs o (x' <|> y)
  where o = maximum (l:map (+1) (freeLocs y ++ boundLocs y ++ filter (/=l) (freeLocs x)))
        x' = relocate l o x
colocate (AbsBase x) (Abs l y) = Abs o (x <|> y')
  where o = maximum (l:map (+1) (freeLocs x ++ boundLocs x ++ filter (/=l) (freeLocs y)))
        y' = relocate l o y
colocate (AbsBase x) (AbsBase y) = AbsBase (x <|> y)

concretify :: Abstraction -> Species
concretify (AbsBase spec) = spec
concretify (Abs l spec) = new [l] spec

-- Pretty print a list of Names.
prettyNames :: (Show a) => [a] -> String
prettyNames ns = L.intercalate "," (map show ns)
