{-# OPTIONS_GHC -XPatternGuards #-}

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
  PrefixSpecies,
  pretty,
  maxLoc
  ) where

import qualified Data.List as L

-- Syntax

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

data Prefix
  = Located Name Location
  | Unlocated Name
  deriving (Eq, Ord, Show)

type NLocation = String

type Location = Integer

data Definition = SpeciesDef Name Species
                  deriving (Show)

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
  | AbsPar [Abstraction]
  | AbsNew [Location] Abstraction
  | AbsBase Species
  deriving (Eq, Ord, Show)

instance Pretty Prefix where
  pretty (Located x l) = x ++ "@" ++ (show l)
  pretty (Unlocated x) = x

instance Pretty Species where
  pretty Nil = "0"
  -- pretty (Def i) = i
  pretty x'@(Sum x@((p,s):pss)) 
    | (length x == 1) 
      = (pretty p) ++ "->" ++ (pretty s)
    | otherwise 
      = (pretty $ Sum [(p,s)]) ++ " + "++(pretty $ Sum pss)
  pretty (Sum []) = "<Empty Sum>"
  pretty x'@(Par x@(s:ss))
    | (null x) 
      = ""
    | (length x == 1) 
      = pretty s
    | otherwise 
      = (pretty s) ++" | "++ (pretty (Par ss))
  pretty (Par []) = "<Empty Par>"
  pretty (New locs spec)
    = "new " ++ (pnames locs) ++ " in " ++ (pretty spec)

-- -- Pretty printing
class (Show a) => Pretty a where
    pretty :: a -> String
    pretty x = show x

instance Pretty Abstraction where
  pretty (Abs l spec)
    = "("++ (show l) ++ ")" ++ "(" ++ (pretty spec) ++ ")"
  pretty (AbsPar ss)
    = concat(L.intersperse " | " (map pretty ss))
  pretty (AbsNew locs spec) 
    = "new " ++ (pnames locs) ++ " in " ++ (pretty spec)
  pretty (AbsBase spec)
    = pretty spec

pnames :: (Show a) => [a] -> String
pnames [l] = show l
pnames [] = ""
pnames (l:ls) = (show l) ++ "," ++ (pnames ls)

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

-- enzymeSpecies :: Species
-- enzymeSpecies = [
--   NSum [

--   ],
-- ]

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

class Nameless a where
  maxLoc :: a -> Location

instance Nameless Species where
  maxLoc Nil = 0
  maxLoc (Par xs@(_:_)) = maximum $ map maxLoc xs
  maxLoc (Par []) = 0
  maxLoc (Sum (x:xs)) = let 
      maxRest = maxLoc (Sum xs)
    in case x of
      (Located _ loc, spec) -> loc `max` maxRest `max` (maxLoc spec)
      (Unlocated _, spec) -> maxRest `max` (maxLoc spec)
  maxLoc (Sum []) = 0
  maxLoc (New locs spec) = maximum ((maxLoc spec):locs)

instance Nameless Abstraction where
  maxLoc (AbsBase spec) = maxLoc spec
  maxLoc (AbsPar specs) = maximum (map maxLoc specs)
  maxLoc (AbsNew locs spec) = maximum locs
  maxLoc (Abs loc spec) = loc

type Env = [Definition]


-- data Prefix = Comm Name
--               deriving (Eq, Ord, Show)

-- data Process = Process [(Species, Conc)] AffNet AffNet
--                deriving (Eq, Ord, Show)

-- data TTau = TTau Rate
--             deriving (Show,Eq,Ord)
-- data TTauAff = TTauAff (Name,Name)
--                deriving (Show,Eq,Ord)

-- data Trans = TransSC Species Name Species  -- A ----a-----> (x;y)B
--            | TransT Species TTau Species      -- A ---t@k----> B
--            | TransTA Species TTauAff Species  -- A -t<a,b>@k-> B
--              deriving (Show,Eq,Ord)

-- data MTS = MTS [Trans]
--        deriving (Show)

-- instance Eq Species where
--   Nil == Nil           = True
--   (Def a) == (Def b)   = a == b
--   (Sum a) == (Sum b)   = a == b
--   (Par a) == (Par b)   = a == b
--   (Free a) == (Free b) = a == b
--   _ == _               = False


-- instance Pretty Process where
--     pretty (Process x@((s,c):scs) n m)
--         | null x = ""
--         | length x == 1
--             = "["++(show c)++"] "++(pretty s)
--         | otherwise
--             = (pretty (Process [(s,c)] n m))++" || "++(pretty (Process scs n m))
--     pretty (Process [] _ _) = "<Empty process>"


-- instance Pretty Aff where
--     pretty (Aff ((n1,n2),r)) = n1++"-"++n2++"@"++(show r)

-- prettyAffs affs = concat(L.intersperse ", " (map pretty affs))

-- instance Pretty AffNet where
--     pretty (AffNet affs) = "{"++(prettyAffs affs)++"}"

-- instance Pretty Definition where
--     pretty (SpeciesDef n s) 
--         = n++" = "++(pretty s)

-- |Pretty print a list of Names.
prettyNames :: [Name] -> String
prettyNames ns = concat(L.intersperse "," ns)

-- Parenthesisation
prettyPs :: Species -> Species -> String
prettyPs x x' 
    | ((prio x)<=(prio x')) 
        = (pretty x)
    | otherwise 
        =  "("++(pretty x)++")"
    where prio Nil = 10
          -- prio (Def _) = 10
          prio (Sum ss)
              | (length ss == 1) = 10
              | otherwise = 20
          prio (Par _) = 30
          -- prio (Free _) = 10


-- instance Pretty MTS where
--     pretty (MTS (t:ts)) = ((pretty t)++"\n")++(pretty (MTS ts))
--     pretty (MTS []) = ""

-- instance Pretty TTau where
--     pretty (TTau r) = "tau@<"++(show r)++">"
-- instance Pretty TTauAff where
--     pretty (TTauAff (n1,n2)) = "tau@<"++n1++","++n2++">"

-- instance Pretty Trans where 
--     pretty (TransSC s n c) 
--         = (pretty s)++" ---"++n++"--> "++(pretty c)
--     pretty (TransT s t s')
--         = prettyTauTrans s t s'
--     pretty (TransTA s t s')
--         = prettyTauTrans s t s'
    
-- prettyTauTrans s t s' = (pretty s)++" ---"++(pretty t)++"--> "++(pretty s')

-- building transition system
-- transitions :: Env -> Species -> MTS

-- testDefs :: Env
-- testDefs = [
--     SpeciesDef "E" (Sum [(Comm "e", Sum [(Comm "ebound", Def "E")])]),
--     SpeciesDef "S" (Sum [(Comm "s", Sum [
--         (Comm "sbound", Free (Def "S")),
--         (Comm "p", Free (Def "P"))
--       ])
--     ]),
--     SpeciesDef "P" (Sum [(Comm "p2", Def "P")])
--   ]

-- trans :: Env -> Species -> MTS
-- trans _ (Free s) = []
-- trans _ Nil = []
-- trans 
-- trans

-- Application of abstractions

-- discharge :: Abstraction -> Location -> Species
-- discharge Abs spec


main = do
  -- putStrLn $ show toNameless (NSum [(NLocated "x" "l", NNil),
  --                                   (NUnlocated "y", "m", NNil)])
  -- putStrLn $ pretty (testDefs!!0)
  -- putStrLn $ pretty (testDefs!!1)
  -- putStrLn $ pretty (testDefs!!2)
  let x = AbsPar [Abs 0 (Par [Sum [(Located "x" 0, AbsBase Nil)],
                                Sum [(Unlocated "y", AbsBase Nil)]]),
                  AbsNew [1] (Abs 0 Nil)]
  let y = Par [Sum [(Unlocated "e", Abs 0 (Sum [(Located "x" 0, AbsBase Nil)]))]]
  let z = Par [Sum [(Unlocated "s", Abs 0 (Sum [(Located "r" 0, AbsBase Nil),
                                                (Located "p" 0, AbsBase Nil)]))]]
  let w = AbsNew [0] (AbsBase (Par [Sum [(Located "x" 0, AbsBase Nil)],
                                    Sum [(Located "r" 0, AbsBase Nil),
                                         (Located "p" 0, AbsBase Nil)]]))
  putStrLn $ show x
  putStrLn $ pretty x
  putStrLn $ show $ maxLoc x
  putStrLn $ show y
  putStrLn $ pretty y
  putStrLn $ show $ maxLoc y
  putStrLn $ show z
  putStrLn $ pretty z
  putStrLn $ show $ maxLoc z
  putStrLn $ show w
  putStrLn $ pretty w
  putStrLn $ show $ maxLoc w