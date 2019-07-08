module BondCalculus.AST
  (
  Abstraction(..),
  Species(..),
  Name,
  Conc,
--   Rate,
  Location,
  Prefix(..),
  Syntax(..),
  ProcessAlgebra(..),
  PrefixSpecies,
  RateLaw(..),
  RateLawFamily,
  Env,
  SpeciesDefinition(..),
  RateLawSpec(..),
  AffinityNetwork,
  AffinityNetworkDefinition(..),
  AffinityNetworkSpec(..),
  Affinity(..),
  ConcreteAffinity(..),
  ConcreteAffinityNetwork,
  RateLawParam(..),
  AbstractProcess(..),
  BondCalculusModel(..),
  KineticLawDefinition(..),
  CombinedModel(..),
  RateLawFn,
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
  mkNew,
  mkProcess,
  denest,
  partitionNew,
  closeSpecs,
  closeLocs,
  canonicallyReorderLocs,
  addSpeciesDef,
  addKineticLawDef,
  addProcessDef,
  addAffinityNetworkDef,
  emptyBondCalculusModel,
  emptyCombinedModel,
  combineModels,
  concretifyKineticLaw,
  prefLoc,
  massAction,
  normalizeProc
  ) where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad
import GHC.Generics
import Data.Hashable
import qualified BondCalculus.Symbolic as Symb
import BondCalculus.Base
-- import Debug.Trace

trace :: a -> b -> b
trace _ b = b

-- import qualified Data.Map as M
data CombinedModel = CombinedModel { modelDouble :: BondCalculusModel Double
                                   , modelInterval :: BondCalculusModel Interval } 

-- Core concepts
type Name = String
type Conc = Double
-- type Rate = Double

-- Rate laws
type RateLawFn a = forall k . (ExpressionOver a k, Num k, Fractional k, Floating k, Eq k, DoubleExpression k) => [k] -> k
newtype RateLaw a = RateLaw (RateLawFn a)
type RateLawFamily a = [a] -> RateLaw a

class (Pretty a, Expression a) => Syntax a where
  relocate :: Location -> Location -> a -> a
  relocateAll :: [Location] -> [Location] -> a -> a
  rename :: Name -> Name -> a -> a
  freeLocs :: a -> [Location]
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

prefLoc :: Prefix -> Maybe Location
prefLoc (Located _ l) = Just l
prefLoc (Unlocated _) = Nothing

type PrefixSpecies = (Prefix, Abstraction)

data RateLawParam a = RateLawParamVar String
                    | RateLawParamVal a
                    deriving (Eq, Ord, Show)

data RateLawSpec a = RateLawAppl String [RateLawParam a]
                --  | RateLawFn RateLaw
                 deriving (Eq, Ord, Show)

data Affinity a = Affinity { affRateLaw :: RateLawSpec a
                           , affSites   :: [[Name]] }
                deriving (Eq, Ord, Show)

instance Arbitrary (RateLawParam Double) where
    arbitrary = oneof [ liftM RateLawParamVar (oneof $ map return ["s", "t", "u"])
                      , liftM RateLawParamVal arbitrary ]

instance Arbitrary (RateLawSpec Double) where
    arbitrary = oneof [ liftM (RateLawAppl "MA" . (:[])) arbitrary
                      , liftM2 (\x y -> RateLawAppl "MM" [x, y]) arbitrary arbitrary ]

instance Arbitrary (Affinity Double) where
    arbitrary = liftM2 Affinity arbitrary arbitrary

data ConcreteAffinity a = ConcreteAffinity { cAffRateLaw :: RateLaw a
                                           , cAffSites   :: [[Name]] }

type AffinityNetwork a = [Affinity a]

type ConcreteAffinityNetwork a = [ConcreteAffinity a]

data AffinityNetworkSpec a = AffinityNetworkAppl String [a]
                           | AffinityNetworkSpec (AffinityNetwork a)
                           | AffinityNetworkCompo (AffinityNetworkSpec a) (AffinityNetworkSpec a)
                           deriving (Ord, Show)

instance Arbitrary (AffinityNetworkSpec Double) where
    arbitrary = sized arbitrary'
        where arbitrary' 0 = oneof [appl, spec]
              arbitrary' n = oneof [appl,
                                    spec,
                                    liftM2 AffinityNetworkCompo (arbitrary' $ n `div` 2)
                                                                (arbitrary' $ n `div` 2)]
              appl = liftM2 AffinityNetworkAppl (oneof $ map return ["A", "B", "C"]) arbitrary
              spec = liftM AffinityNetworkSpec arbitrary

instance Ord a => Eq (AffinityNetworkSpec a) where
    l == m = normalizeAffinityNetworkSpec l == normalizeAffinityNetworkSpec m

normalizeAffinityNetworkSpec (AffinityNetworkCompo x y) = (L.sort (xs1 ++ ys1), L.sort (xs2 ++ ys2))
    where (xs1, xs2) = normalizeAffinityNetworkSpec x
          (ys1, ys2) = normalizeAffinityNetworkSpec y
normalizeAffinityNetworkSpec (AffinityNetworkAppl s rs) = ([(s, rs)], [])
normalizeAffinityNetworkSpec (AffinityNetworkSpec ys) = ([], L.sort ys)

instance Semigroup (AffinityNetworkSpec a) where
    AffinityNetworkSpec xs <> AffinityNetworkSpec ys = AffinityNetworkSpec (xs ++ ys)
    x <> y = AffinityNetworkCompo x y

instance Monoid (AffinityNetworkSpec a) where
    mempty = AffinityNetworkSpec []

data AbstractProcess a =
    Process (AffinityNetworkSpec a) [(a, Species)]
  | ProcessAppl String
  | ProcessCompo (AbstractProcess a) (AbstractProcess a)
  deriving (Ord, Show)

mkProcess :: Num a => AffinityNetworkSpec a -> [(a, Species)] -> AbstractProcess a
mkProcess x ys = Process x (normalizeConcSpecs ys)

normalizeConcSpecs :: Num a => [(a, Species)] -> [(a, Species)]
normalizeConcSpecs ys = [ (sum [c | (c, w) <- ys, w == s], s)
                        | s <- L.sort $ L.nub $ map snd ys ]

type ProcNF a = ([String], AffinityNetworkSpec a, [(a, Species)])

normalizeProc :: Num a => AbstractProcess a -> ProcNF a
normalizeProc (Process x ys) = ([], x, normalizeConcSpecs ys)
normalizeProc (ProcessAppl x) = ([x], mempty, [])
normalizeProc (ProcessCompo p1 p2) = (L.sort (ps1 ++ ps2),
                                      x1 <> x2,
                                      normalizeConcSpecs (ys1 ++ ys2))
    where (ps1, x1, ys1) = normalizeProc p1
          (ps2, x2, ys2) = normalizeProc p2

instance (Num a, Eq a, Ord a) => Eq (AbstractProcess a) where
    Process x1 ys1 == Process x2 ys2 = x1 == x2 && normalizeConcSpecs ys1 == normalizeConcSpecs ys2

instance Num a => Semigroup (AbstractProcess a) where
    Process x1 ys1 <> Process x2 ys2 = mkProcess (x1 <> x2) (ys1 ++ ys2)
    x <> y = ProcessCompo x y

instance Num a => Monoid (AbstractProcess a) where
    mempty = mkProcess mempty []

data SpeciesDefinition = SpeciesDef
  { specArgs :: [Name]
  , specLocs :: [Location]
  , specBody :: Species }
  deriving (Eq, Ord, Show)

data AffinityNetworkDefinition a = AffinityNetworkDef
  { affArgs :: [Name]
  , affBody :: AffinityNetwork a }
  deriving (Eq, Ord, Show)

data KineticLawDefinition a = KineticLawDef
  { kinParms :: [Name]
  , kinArgs :: [Name]
  , kinBody :: Symb.SymbolicExpr a }
  deriving (Eq, Ord, Show)

concretifyKineticLaw :: forall a . Symb.ExprConstant a => KineticLawDefinition a -> RateLawFamily a
concretifyKineticLaw (KineticLawDef params args body) params' = RateLaw f
  where --f :: ExpressionOver k a => [k] -> k 
        f :: RateLawFn a
        f args' = case Symb.eval (M.fromList $ zip args args') body' of
          Left err -> error $ concat err
          Right x  -> x
        body' :: Symb.SymbolicExpr a
        body' = simplify $ Symb.applyVar (M.fromList $ zip params (map val params')) body

data BondCalculusModel a = Defs
  { speciesDefs         :: Env
  , affinityNetworkDefs :: Map String (AffinityNetworkDefinition a)
  , kineticLawDefs      :: Map String (RateLawFamily a)
  , processDefs         :: Map String (AbstractProcess a) }

instance Show a => Show (BondCalculusModel a) where
  show (Defs s a _ p) = "Defs " ++ show s ++ " "
                                ++ show a ++ " "
                                ++ " M.empty "
                                ++ show p

instance (Num a, Eq a, Ord a) => Eq (BondCalculusModel a) where
  m == n = speciesDefs m         == speciesDefs n
        && affinityNetworkDefs m == affinityNetworkDefs n
        && processDefs m         == processDefs n

addSpeciesDef :: String -> SpeciesDefinition -> BondCalculusModel a -> BondCalculusModel a
addSpeciesDef name def (Defs s a k p) = Defs (M.insert name def s) a k p

addAffinityNetworkDef :: String
                      -> AffinityNetworkDefinition a
                      -> BondCalculusModel a
                      -> BondCalculusModel a
addAffinityNetworkDef name def (Defs s a k p) = Defs s (M.insert name def a) k p

addKineticLawDef :: String -> RateLawFamily a -> BondCalculusModel a -> BondCalculusModel a
addKineticLawDef name def (Defs s a k p) = Defs s a (M.insert name def k) p

addProcessDef :: String -> AbstractProcess a -> BondCalculusModel a -> BondCalculusModel a
addProcessDef name def (Defs s a k p) = Defs s a k (M.insert name def p)

combineModels :: BondCalculusModel a -> BondCalculusModel a -> BondCalculusModel a
combineModels (Defs s1 a1 k1 p1) (Defs s2 a2 k2 p2) = Defs (s1 `M.union` s2)
                                                           (a1 `M.union` a2)
                                                           (k1 `M.union` k2)
                                                           (p1 `M.union` p2)

massAction :: Symb.ExprConstant a => RateLawFamily a
massAction [m] = RateLaw $ \xs -> product (val m:xs)
massAction _ = error "Wrong number of parameters provided to mass action kinetic law"

emptyBondCalculusModel :: Symb.ExprConstant a => BondCalculusModel a
emptyBondCalculusModel = Defs { speciesDefs = M.empty
  -- Mass action kinetic law is always available
                     , kineticLawDefs = M.fromList [("MA", massAction)]
                     , affinityNetworkDefs = M.empty
                     , processDefs = M.empty }

emptyCombinedModel :: CombinedModel
emptyCombinedModel = CombinedModel emptyBondCalculusModel emptyBondCalculusModel

type Env = Map String SpeciesDefinition

type ExpId = Int

type NF = Species

data Species = Nil
             | Sum ExpId NF ![PrefixSpecies]
             | Par ExpId NF ![Species]
             | New ExpId NF ![Location] !Species
             | Def !String ![Name] ![Location]

instance Show Species where
  show Nil = "Nil"
  show (Par _ _ xs) = "mkPar " ++ show xs
  show (New _ _ locs x) = "mkNew " ++ show locs ++ " (" ++ show x ++ ")"
  show (Sum _ _ xs) = "mkSum " ++ show xs
  show (Def nm args locs) = "Def " ++ show nm ++ " " ++ show args ++ " "
                            ++ show locs

instance Arbitrary Prefix where
  arbitrary = oneof [fmap Unlocated (elements ["x", "y", "z"]),
                     liftM2 Located (elements ["x", "y", "z"])
                                    (choose (0,3))]
  shrink (Located x l) = [Unlocated x, Located x (l-1)]
  shrink (Unlocated _) = []

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

  shrink Nil = []
  shrink (Par _ _ xs) = Nil:[mkPar xs' | xs' <- shrink xs]
  shrink (New _ _ locs x) = [Nil, x] ++ [mkNew ls x | ls <- shrink locs] ++ map (mkNew locs) (shrink x)
  shrink (Def name args locs) = [Nil] ++ [Def name args' locs | args' <- tails args] ++ [Def name args locs' | locs' <- shrink locs]
    where tails [] = []
          tails s = map tail $ L.permutations s
  shrink (Sum _ _ xs) = Nil:[mkSum xs' | xs' <- shrink xs]

instSpec :: SpeciesDefinition -> [Name] -> [Location] -> Species
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

instance Show Abstraction where
  show (Abs _ loc spec) = "mkAbs " ++ show loc ++ " (" ++ show spec ++ ")"
  show (AbsBase _ spec) = "mkAbsBase (" ++ show spec ++ ")"

mkAbs :: Location -> Species -> Abstraction
mkAbs loc spec = Abs expid loc spec
  where expid = hash (1::Int, loc, spec)

mkAbsBase :: Species -> Abstraction
mkAbsBase spec = AbsBase expid spec
  where expid = hash (2::Int, 1::Int, spec)

instance Hashable Species where
  hash Nil = hash(0::Int,0::Int,0::Int,0::Int)
  hash (Def s nms locs) = hash(1::Int,s,nms,locs)
  hash (Sum expid _ _) = expid
  hash (Par expid _ _) = expid
  hash (New expid _ _ _) = expid

  hashWithSalt salt spec = hash (hash salt, hash spec)

mkSum :: [PrefixSpecies] -> Species
mkSum xs = res
  -- trace ("making sum " ++ pretty res ++ " from " ++ concatMap (\(x, y) -> pretty x ++ "->" ++ pretty y) xs) res
  where expid = hash(2::Int,map hash xs,0::Int,0::Int)
        res = Sum expid nf xs
        nf = normalForm' res

mkPar :: [Species] -> Species
mkPar xs = res
  -- trace ("making par " ++ pretty res ++ " from " ++ show (map pretty xs)) res
  where expid = hash(3::Int,map hash xs,0::Int,0::Int)
        res = Par expid nf xs
        nf = normalForm' res

mkNew :: [Location] -> Species -> Species
mkNew locs xs = ans
 -- trace ("making new from " ++ show locs ++ " and " ++ pretty xs)
  where expid = hash(4::Int,locs,xs,0::Int)
        -- nf = normalForm' res
        ans = let res = New expid nf locs xs
                  nf  = normalForm' res
              in res

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
  shrink (AbsBase _ x) = [mkAbsBase x' | x' <- shrink x]
  shrink (Abs _ l x)   = [mkAbsBase Nil, mkAbsBase x, mkAbs (l-1) x] ++ [mkAbs l x' | x' <- shrink x]

instance Expression Prefix where

instance Expression [Prefix] where
  simplify ps = L.sort $ map simplify ps

instance Syntax Prefix where
  relocate l l' p@(Located x m)
    | l == m = Located x l'
    | otherwise = p
  relocate _ _ p@(Unlocated _) = p

  relocateAll ls ls' p@(Located x l) = case L.elemIndex l ls of
    Just i  -> Located x (ls' !! i)
    Nothing -> p
  relocateAll _ _ p@Unlocated{} = p

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

  relocateAll _ _ Nil = Nil
  relocateAll !ls !ls' (Sum _ _ xs)
    = mkSum [(relocateAll ls ls' pre, relocateAll ls ls' x)
          | (pre, x) <- xs]
  relocateAll !ls !ls' (Par _ _ xs) = mkPar $ map (relocateAll ls ls') xs
  -- should never be used to capture variables!
  relocateAll !ls !ls' (New _ _ locs spec) = if null $ L.intersect locs ls'
    then mkNew locs $! relocateAll ms ms' spec
    else error $ "Trying to rewrite ls = " ++ show ls ++ " to ls' = " ++ show ls' ++ " under locs = " ++ show locs ++ " -- not allowed as this would cause name capture!"
    where !ms  = filter (`notElem` locs) ls
          !ms' = map (ls' !!) $ L.findIndices (`elem` ms) ls
  relocateAll !ls !ls' (Def name args locs) = Def name args (map rl locs)
    where rl !l = case L.elemIndex l ls of
                    Just i  -> ls' !! i
                    Nothing -> l

  rename _ _ Nil = Nil
  rename x x' (Sum _ _ prefspecs) = mkSum [(rename x x' pre, rename x x' spec)
                                    | (pre, spec) <- prefspecs]
  rename x x' (Par _ _ specs) = mkPar $ map (rename x x') specs
  rename x x' (New _ _ ls spec) = mkNew ls $ rename x x' spec
  rename x x' (Def name args locs) = Def name [if y == x then x'
                                               else y | y <- args] locs

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

instance Expression Species where
  simplify = normalForm

normalForm' :: Species -> Species
normalForm' Nil = Nil
normalForm' d@Def{} = d
-- normalForm (Def expid _ args locs) = let nf = Def expid _ args locs
--                                      in nf
normalForm' (Sum _ _ []) = Nil
normalForm' sm@(Sum _ _ ss) = if nfss == ss then sm else mkSum nfss
  where nfss = L.sort [(pref, normalForm s) | (pref, s) <- ss]
normalForm' (Par _ _ [pr@Par{}]) = trace ("unwrapping" ++ pretty pr) $ normalForm pr
normalForm' pr@(Par _ _ ss) = res
  -- trace ("normalizing pars " ++ pretty pr ++ "(" ++ show(expId pr) ++ ") to " ++ pretty res ++ "(" ++ show(expId res) ++ ")")
  where res = case ss' of
          []  -> Nil
          [s] -> s
          _   -> if ss == ss' then pr else res'
          -- trace ("bottom at pr = " ++ pretty pr ++ " given ss' = " ++ show (map pretty ss') ++ " ss = " ++ show (map pretty ss))
        res' = mkPar ss'
        flatten    = L.concatMap f
        f (Par _ _ ps) = ps
        f s        = [s]
        ss' = L.sort $ filter (/=Nil) $ flatten $ map normalForm ss
normalForm' spec@(New _ _ locs s)
  | null locs''' = trace ("Unwrapping to s' = " ++ pretty s') s'''
  | (locs''' /= locs || s /= s''') && length part == 1 = trace "reapplying with s'''" $ normalForm $ mkNew locs''' s'''
  | length part > 1 = partNf
  -- trace ("locs = " ++ show locs ++ ", locs''' = " ++ show locs''' ++ ", eq = " ++ show (locs == locs''') ++ "\ns = " ++ show s ++ ", s'''' = " ++ show s'''' ++ ", eq = " ++ show (s == s'''') ++  "\npart = " ++ show part)
  | s'''' /= s = mkNew locs''' s''''
  | otherwise = spec
  where (locs', s')     = trace ("--- normal form of " ++ pretty spec) $ denest locs s
        locs''          = L.sort $ L.nub $ L.intersect locs' (freeLocs s')
        (locs''', s'')  = trace ("s'' for spec = " ++ show spec) $ reduceLocs locs'' (normalForm s')
        s'''            = trace("s''' for spec = " ++ show spec) $ normalForm s''
        s''''           = trace("canonically ordering locs''' = " ++ show locs''' ++ " in s''' = " ++ show s''') $ canonicallyReorderLocs locs''' s'''
        -- (locs'''', s''''')  = reduceLocs locs''' (normalForm s'''')
        part            = case s''' of
                            Par _ _ sp -> trace ("partitioning sp = " ++ show sp ++ " to " ++ show p) p
                              where p = partitionNew locs''' sp
                            sp         -> [(locs''', [sp])]
        partNf          = normalForm $ mkPar [mkNew ls (mkPar sp) | (ls, sp) <- part]

-- utility functions for rewriting towards normal form
reduceLocs :: [Location] -> Species -> ([Location], Species)
reduceLocs olocs m = (nLocs, if olocs == nLocs then m
                             else normalForm m')
  where fLocs = filter (`notElem` olocs) (freeLocs m)
        bLocs = boundLocs m
        sLoc = if null bLocs then 0 else maximum bLocs + 1
        nextLocs = filter (`notElem` fLocs) [sLoc..]
        locLen = length olocs
        nLocs = take locLen nextLocs
        m' = relocateAll olocs nLocs m

-- places the given location within a species within canonical order
canonicallyReorderLocs :: [Location] -> Species -> Species
canonicallyReorderLocs locs m = if length locs < 2 || m' == m
                                then m else m'
  where locPerms = L.permutations locs
        -- compute this way, one more normal form computation required,
        -- but much less memory
        m's = [(hash $! reorder locs', reorder locs') | locs' <- locPerms]
        m' =  snd (head $ L.sortOn fst m's)
        reorder locs' = normalForm $ relocateAll locs locs' m
        -- trace("reordering based on "
        --   ++ show [(hash m'', pretty m'') | m'' <- m's])

denest :: [Location] -> Species -> ([Location], Species)
denest ls pr@(Par _ _ ss) = case news of
  []            -> (ls, pr)
  (ms, s, rs):_ -> denest (L.sort $ ls ++ ms') pr'
    where nextLocs = filter (`notElem` aLocs) [0..]
          rLocs    = concatMap freeLocs rs ++ concatMap boundLocs rs
          sLocs    = freeLocs s ++ boundLocs s
          aLocs    = rLocs ++ sLocs ++ ls
          ms'      = take (length ms) nextLocs
          s'      = relocateAll ms ms' s
          pr'      = normalForm (mkPar (s':rs))
  where news = [(ms, s, [w | (j, w) <- iss, j /= i])
               | (i, New _ _ ms s) <- iss]
        iss = zip ([0..]::[Integer]) ss
denest ls r@New{} = denest ls $ mkPar [r]
denest ls s = (ls, s)

closeLocs :: [Location] -> [Location] -> [Species] -> [Species]
          -> ([Location], [Location], [Species], [Species])
closeLocs lls rls lss rss = case newLls of
  [] -> (lls, rls, lss, rss)
  _  -> closeSpecs (lls++newLls) (filter (`notElem` newLls) rls) lss rss
  where newLls = L.nub $ concat [freeLocs s `L.intersect` rls | s <- lss]

closeSpecs :: [Location] -> [Location] -> [Species] -> [Species]
           -> ([Location], [Location], [Species], [Species])
closeSpecs lls rls lss rss = case newLss of
  [] -> (lls, rls, lss, rss)
  _  -> closeLocs lls rls (lss++newLss) (filter (`notElem` newLss) rss)
  where newLss = filter (\s -> not $ null $ freeLocs s `L.intersect` lls) rss

partitionNew :: [Location] -> [Species] -> [([Location], [Species])]
partitionNew [] ss     = [([], ss)]
-- partitionNew ls []     = [(ls, [])]
partitionNew (l:ls) ss = if null rls && null rss then [(lls, lss)]
                         else (lls, lss) : partitionNew rls rss
  where (lls, rls, lss, rss) = closeSpecs [l] ls [] ss

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
  priority Par{} = 30
  priority New{} = 40

instance Nameless Species where
  maxLoc xs = if null locs then 0 else maximum locs
    where locs = boundLocs xs

instance Pretty Species where
  pretty Nil = "0"
  pretty (Sum _ _ x@((p,s):pss))
    | length x == 1
      = pretty p ++ "->" ++ if priority s < 30 then pretty s
                                                else "(" ++ pretty s ++ ")"
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

  relocateAll ls ls' (AbsBase _ spec) = mkAbsBase $ relocateAll ls ls' spec
  -- should never be used to capture variables!
  relocateAll ls ls' (Abs _ m abst) =
    if m `elem` ls'
    then error $ "Trying to relocate ls = " ++ show ls ++ " to ls' = " ++
          show ls' ++ " which would capture m = " ++ show m ++ "!"
    else mkAbs m $! relocateAll ms ms' abst
    where !ms  = filter (/=m) ls
          !ms' = map (ls' !!) $ L.findIndices (`elem` ms) ls

  rename x x' (AbsBase _ spec) = mkAbsBase $ rename x x' spec
  rename x x' (Abs _ m abst) = mkAbs m $ rename x x' abst

  freeLocs (AbsBase _ spec) = freeLocs spec
  freeLocs (Abs _ m abst) = filter (/=m) $ freeLocs abst

  normalForm (AbsBase _ x) = mkAbsBase (normalForm x)
  normalForm abst@(Abs _ l x) = if l `notElem` freeLocs x'
                                then mkAbsBase x'
                                else abst'
    where x' = normalForm x
          bLocs = boundLocs x'
          sLoc = if null bLocs then 0 else maximum bLocs + 1
          fLocs = filter (/=l) $ freeLocs x'
          nextLocs = filter (`notElem` fLocs) [sLoc..]
          l' = head nextLocs
          abst' = if l == l' && x == x' then abst
                  else trace("rewriting abst = " ++ pretty abst ++ " to " ++ pretty (mkAbs l' $ normalForm $ relocate l l' x')) $ normalForm $ mkAbs l' $ normalForm $ relocate l l' x'

instance Expression Abstraction where
  simplify = normalForm

instance ProcessAlgebra Abstraction where
  (<|>) = colocate

  new locs (AbsBase _ spec) = mkAbsBase $ new locs spec
  new locs (Abs _ m spec) = mkAbs m $ new locs spec

  priority (AbsBase _ spec) = priority spec
  priority Abs{} = 9

  boundLocs (Abs _ l spec) = L.nub $ L.sort $ l:boundLocs spec
  boundLocs (AbsBase _ spec) = L.nub $ L.sort $ boundLocs spec

instance Nameless Abstraction where
  maxLoc xs = if null locs then 0 else maximum locs
    where locs = boundLocs xs

instance Pretty Abstraction where
  pretty (Abs _ l spec)
    = "("++ show l ++ ")" ++ if priority spec < 30 then pretty spec
                             else "(" ++ pretty spec ++ ")"
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

instance Pretty [Prefix] where
  pretty prefs = L.intercalate "," (map pretty prefs)
