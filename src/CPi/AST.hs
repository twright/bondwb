module CPi.AST
  (
  -- MTS(..),
  -- Trans(..),
  -- TTauAff(..),
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
  concretify,
  pretty,
  maxLoc,
  prettyNames,
  prettyParens,
  colocate,
  ) where

import qualified Data.List as L

-- Core concepts
type Name = String
type Conc = Double
type Rate = Double

-- Rate laws
type RateLaw = [Conc] -> Rate
type RateLawFamily = [Double] -> RateLaw

-- Affinity networks
-- data Aff = Aff ([[Name]], RateLaw)
--            deriving (Eq, Ord, Show)
-- data AffNet = AffNet [Aff]
--               deriving (Eq, Ord, Show)

-- Prefixes

-- type NLocation = String

-- Pretty printing
class (Show a) => Pretty a where
  pretty :: a -> String
  pretty = show

class (Pretty a) => Syntax a where
  relocate :: Location -> Location -> a -> a
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

data Definition = SpeciesDef Name Species
                  deriving (Show)

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

data Species = Nil
             | Sum [PrefixSpecies]
             | Par [Species]
             | New [Location] Species
             -- | Free Species
             -- | Def String
               deriving (Eq, Ord, Show)

-- Abstractions are formed form species but allow abstracting over
-- variable name to allow for application (when a potential interaction)
-- is finalized.
data Abstraction
  = Abs Location Species
  -- | AbsPar [Abstraction]
  -- | AbsNew [Location] Abstraction
  | AbsBase Species
  deriving (Eq, Ord, Show)

instance Syntax Prefix where
  relocate l l' p@(Located x m)
    | l == m = Located x l'
    | otherwise = p
  relocate _ _ p@(Unlocated _) = p

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

  freeLocs Nil = []
  freeLocs (Sum ss) = L.concat [freeLocs pref ++ freeLocs abst
    | (pref, abst) <- ss]
  freeLocs (Par ss) = foldr ((++) . freeLocs) [] ss
  freeLocs (New locs s) = filter (`notElem` locs) $ freeLocs s

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
  -- relocate l l' (AbsNew locs abst) = AbsNew locs $ relocate l l' abst
  -- relocate l l' (AbsPar xs) = AbsPar $ map (relocate l l') xs

  freeLocs (AbsBase spec) = freeLocs spec  
  freeLocs (Abs m abst) = filter (/=m) $ freeLocs abst 

  simplify (AbsBase x) = AbsBase (simplify x)
  simplify (Abs l x) = Abs l (simplify x)

instance ProcessAlgebra Abstraction where
  -- AbsPar xs <|> AbsPar ys = AbsPar (xs ++ ys)
  -- x <|> AbsPar ys = AbsPar (x:ys)
  -- AbsPar xs <|> y = AbsPar (xs ++ [y])
  -- x <|> y = AbsPar [x, y]
  (<|>) = colocate

  new locs (AbsBase spec) = AbsBase $ new locs spec
  new locs (Abs m spec) = Abs m $ new locs spec

  priority (AbsBase spec) = priority spec
  priority (Abs _ _) = 11
  -- priority (AbsNew _ _) = 40
  -- priority (AbsPar []) = 10
  -- priority (AbsPar [x]) = succ $ priority x
  -- priority (AbsPar _) = 30

instance Nameless Abstraction where
  maxLoc (AbsBase spec) = maxLoc spec
  maxLoc (Abs loc _) = loc
  -- maxLoc (AbsPar specs) = maximum (map maxLoc specs)
  -- maxLoc (AbsNew locs _) = maximum locs

instance Pretty Abstraction where
  pretty x@(Abs l spec)
    = "("++ show l ++ ")" ++ prettyParens spec x
  pretty (AbsBase spec)
    = pretty spec
  -- pretty par@(AbsPar abss) = prettyPar abss
  --   where prettyPar :: [Abstraction] -> String
  --         prettyPar [] = "<Empty AbsPar>"
  --         prettyPar [x] = prettyParens x par 
  --         prettyPar (x:xs) = prettyParens x par ++ " | " ++ prettyPar xs
  -- pretty (AbsNew locs spec) 
  --   = "new " ++ prettyNames locs ++ " in " ++ pretty spec

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

type Env = [Definition]

-- Pretty print a list of Names.
prettyNames :: (Show a) => [a] -> String
prettyNames ns = L.intercalate "," (map show ns)

-- Relocate unbound


-- Take two abstractions and colocate them


-- Application of abstractions

-- discharge :: Abstraction -> Location -> Species
-- discharge Abs spec

-- instance Pretty Prefix where
--     -- pretty (Tau r) = "tau<"++(show r)++">."
--     pretty (Comm n) = n++"."

-- data NPrefix
--   = NLocated (Name, NLocation)
--   | NUnlocated Name
-- type NPrefixSpecies = (NPrefix, NSpecies)

-- data NSpecies = NNil
--              | NSum [NPrefixSpecies]
--              | NPar [Species]
--              | New [NLocation] NSpecies
--              -- | Free Species
--              -- | Def String
--                deriving (Ord, Show)

-- toNameless :: NSpecies -> Species
-- toNameless NNil = Nil
-- toNameless NPar specs = Par (map toNameless specs)
-- toNameless NSum xs = case x of
--   NLocated (name, _)  -> Sum ((Located name n'):ys)
--   NUnlocated name     -> Sum ((Unlocated name):ys)
--   where ys = case toNameless (NSum xs) of
--     Sum yss -> yss
--     toNamelessL [] = []
--     toNamelessL (x:xs) = case x of
--       NLocated name _ -> Located name (n + 1)
--       NUnlocated name -> Unlocated name
--       where ys = toNamelessL xs
--             n = maxLoc (Sum ys)
--             n' = succ n
