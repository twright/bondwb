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

class (Syntax a) => ProcessAlgebra a where
  (<|>) :: a -> a -> a
  -- Priority for adding parenthesis in pretty printing
  priority :: a -> Integer
  new :: [Location] -> a -> a

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
               deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

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
  relocate l l' (New ls spec) = New ls $ relocate l l' spec
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

  simplify Nil = Nil
  simplify (Sum []) = Nil
  simplify (Sum ss) = Sum $ L.sort [(pref, simplify s) | (pref, s) <- ss]
  simplify (Par []) = Nil
  simplify (Par [x]) = simplify x
  simplify (Par xs) = Par $ L.sort $ filter (/=Nil) $ map simplify xs
  simplify (New locs s)
    | null locs' = s'
    | otherwise = New locs' s'
    where s' = simplify s
          locs' = L.sort $ L.nub $ L.intersect locs $ freeLocs s'
  simplify d@Def{} = d

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

  new newlocs (New locs spec) = new (newlocs ++ locs) spec
  new newlocs spec
    | null newlocs' = spec
    | otherwise = New newlocs' spec
    where newlocs' = L.nub $ L.intersect newlocs (freeLocs spec)

  priority Nil = 5
  priority (Sum ss)
    | length ss == 1 = 10
    | otherwise = 30
  priority (Par []) = 10
  priority (Par [x]) = succ $ priority x
  priority (Par _) = 30
  priority (New _ _) = 40

instance Nameless Species where
  maxLoc Nil = 0
  maxLoc (Par xs@(_:_)) = maximum $ map maxLoc xs
  maxLoc (Par []) = 0
  maxLoc (Sum (x:xs)) = let
      maxRest = maxLoc (Sum xs)
    in case x of
      (Located _ loc, spec) -> loc `max` maxRest `max` maxLoc spec
      (Unlocated _, spec) -> maxRest `max` maxLoc spec
  maxLoc (Sum []) = 0
  maxLoc (New locs spec) = maximum (maxLoc spec : locs)
  maxLoc (Def _ _ []) = 0
  maxLoc (Def _ _ locs) = maximum locs

instance Pretty Species where
  pretty Nil = "0"
  -- pretty (Def i) = i
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


instance Syntax Abstraction where
  relocate l l' (AbsBase spec) = AbsBase $ relocate l l' spec
  relocate l l' (Abs m abst) = Abs m $ relocate l l' abst

  rename x x' (AbsBase spec) = AbsBase $ rename x x' spec
  rename x x' (Abs m abst) = Abs m $ rename x x' abst

  freeLocs (AbsBase spec) = freeLocs spec
  freeLocs (Abs m abst) = filter (/=m) $ freeLocs abst

  simplify (AbsBase x) = AbsBase (simplify x)
  simplify (Abs l x) = Abs l (simplify x)

instance ProcessAlgebra Abstraction where
  (<|>) = colocate

  new locs (AbsBase spec) = AbsBase $ new locs spec
  new locs (Abs m spec) = Abs m $ new locs spec

  priority (AbsBase spec) = priority spec
  priority (Abs _ _) = 11

instance Nameless Abstraction where
  maxLoc (AbsBase spec) = maxLoc spec
  maxLoc (Abs loc _) = loc

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
  where o = max l (maxLoc y)
        x' = relocate l o x
colocate (AbsBase x) (Abs l y) = Abs o (x <|> y')
  where o = max (maxLoc x) l
        y' = relocate l o y
colocate (AbsBase x) (AbsBase y) = AbsBase (x <|> y)

concretify :: Abstraction -> Species
concretify (AbsBase spec) = spec
concretify (Abs l spec) = new [l] spec

-- Pretty print a list of Names.
prettyNames :: (Show a) => [a] -> String
prettyNames ns = L.intercalate "," (map show ns)
