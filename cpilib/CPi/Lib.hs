-- (C) Copyright Chris Banks 2011

-- This file is part of The Continuous Pi-calculus Workbench (CPiWB).

--     CPiWB is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.

--     CPiWB is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.

--     You should have received a copy of the GNU General Public License
--     along with CPiWB.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module CPi.Lib
    (Species(..),
     Process(..),
     Prefix(..),
     PrefixSpecies,
     AffNet(..),
     Aff(..),
     Definition(..),
     Nf(..),
     Conc,
     Env,
     Name,
     OutNames,
     InNames,
     Rate,
     lookupDef,
     revLookupDef,
     lookupProcName,
     lookupSpecName,
     speciesInProc,
     compose,
     prettyNames,
     aff,
     sites,
     sub,
     netUnion,
     fn, bn,
     rename,
     vecRename,
     netRename,
     specName,
     Pretty(..),
     (#),(##)
     ) where

import qualified Data.List as L
--import qualified Data.Map as Map
--import Data.Map (Map)
import qualified Control.Exception as X

import BioCalcLib.Lib

----------------------
--Data structures:
-----------------------

type Name = String
type OutNames = [Name]
type InNames = [Name]
type Rate = Double
type Conc = Double
type PrefixSpecies = (Prefix,Species)
infty :: Double
infty = 1/0

data Prefix = Comm Name OutNames InNames
            | Tau Rate
              deriving (Eq, Ord, Show)

newtype Aff = Aff ((Name,Name),Rate)
         deriving(Eq, Ord, Show)

newtype AffNet = AffNet [Aff]
         deriving(Eq, Ord, Show)

data Species = Nil
             | Def String [Name]
             | Sum [PrefixSpecies]
             | Par [Species]
             | New AffNet Species
               deriving (Ord, Show)

data Process = Process [(Species,Conc)] AffNet
               deriving (Eq, Ord, Show)

data Definition = SpeciesDef Name [Name] Species
                | ProcessDef Name Process
                  deriving (Eq,Show)

type Env = [Definition] -- Env(ironment) is a list of definitions

-- Referential equality of Species (e.g. for lookup).
instance Eq Species where
    Nil == Nil                 =  True
    (Def s ns) == (Def s' ns') =  (s==s')&&(ns==ns')
    (Sum ss) == (Sum ss')      =  ss==ss'
    (Par ss) == (Par ss')      =  ss==ss'
    (New n s) == (New n' s')   =  (n==n')&&(s==s')
    _ == _                     =  False

----------------------
-- Pretty printing:
----------------------
-- Instances of Pretty class are pretty-printable
-- CPi components.
-- class (Show a) => Pretty a where
--     pretty :: a -> String
--     pretty = show

instance Pretty Process where
    pretty (Process x@((s,c):scs) n)
        | null x = ""
        | length x == 1
            = "["++show c++"] "++pretty s
        | otherwise
            = pretty (Process [(s,c)] n)++" || "++pretty (Process scs n)
    pretty (Process [] _) = "<Empty process>"

instance Pretty Species where
    pretty Nil = "0"
    pretty (Def i ns) = i ++"("++prettyNames ns++")"
                          -- NOTE: temp removed def params!
    pretty x'@(Sum x@((p,s):pss))
        | length x == 1
            = pretty p++prettyPs s x'
        | otherwise
            = pretty (Sum [(p,s)])++" + "++pretty (Sum pss)
    pretty (Sum []) = "<Empty Sum>"
    pretty x'@(Par x@(s:ss))
        | null x
            = ""
        | length x == 1
            = pretty s
        | otherwise
            = prettyPs s x'++" | "++prettyPs (Par ss) x'
    pretty (Par []) = "<Empty Par>"
    pretty (New n s) = pretty n++" "++pretty s

instance Pretty Prefix where
    pretty (Tau r) = "tau<"++show r++">."
    pretty (Comm n [] []) = n++"."
    pretty (Comm n [] is) = n++"("++prettyNames is++")."
    pretty (Comm n os []) = n++"<"++prettyNames os++">."
    pretty (Comm n os is) = n++"("++prettyNames os++";"
                            ++prettyNames is++")."

instance Pretty Aff where
    pretty (Aff ((n1,n2),r)) = n1++"-"++n2++"@"++(show r)

prettyAffs :: Pretty a => [a] -> String
prettyAffs affs = concat(L.intersperse ", " (map pretty affs))

instance Pretty AffNet where
    pretty (AffNet affs) = "{"++(prettyAffs affs)++"}"

instance Pretty Definition where
    pretty (SpeciesDef n fns s)
        = n++"("++(prettyNames fns)++") = "++(pretty s)
    pretty (ProcessDef  n p)
        = n++" = "++(pretty p)

-- Ordering for Definitions:
instance Ord Definition where
    -- Any SpeciesDef < any ProcessDef
    compare (SpeciesDef _ _ _) (ProcessDef _ _)    = LT
    compare (ProcessDef _ _)   (SpeciesDef _ _ _)  = GT
    -- Both ProcessDef and SpeciesDef sort by name.
    compare (SpeciesDef n _ _) (SpeciesDef n' _ _) = compare n n'
    compare (ProcessDef n _)   (ProcessDef n' _)   = compare n n'

-- |Pretty print a list of Names.
prettyNames :: [Name] -> String
prettyNames = L.intercalate ","

-- Parenthesisation
prettyPs :: Species -> Species -> String
prettyPs x x'
    | (prio x :: Integer) <= prio x'
        = pretty x
    | otherwise
        =  "("++pretty x++")"
    where prio Nil = 10
          prio (Def _ _) = 10
          prio (Sum ss)
              | length ss == 1 = 10
              | otherwise = 20
          prio (Par _) = 30
          prio (New _ _) = 40

----------------------
--Functions:
----------------------

--AffNet funs:
sites :: AffNet -> [Name]
sites net = sites' net []
    where
      sites' :: AffNet -> [Name] -> [Name]
      sites' (AffNet (Aff ((s1,s2),_):affs)) r
          = sites' (AffNet affs) (s1:s2:r)
      sites' (AffNet []) r = L.nub r

aff :: AffNet -> (Name,Name) -> Maybe Rate
aff (AffNet affs) (n,n')
    | a /= Nothing = a
    | otherwise = aff' (n',n) affs
    where a = aff' (n,n') affs
          aff' _ []          =  Nothing
          aff' k (Aff ((x,y),r):affs')
              | k == (x,y) =  Just r
              | otherwise  =  aff' k affs'

netUnion :: AffNet -> AffNet -> AffNet
netUnion (AffNet n) (AffNet n') = AffNet (n \/ n')

--Free/Bound names:
fn :: Species -> [Name]
fn Nil = []
fn (Def _ ns) = ns
fn (Par []) = []
fn (Par (x:xs)) = (fn x) \/ (fn (Par xs))
fn (New n s) = (fn s) \\ (sites n)
fn (Sum []) = []
fn (Sum (((Tau _),s):xs)) = (fn s) \/ (fn (Sum xs))
fn (Sum (((Comm n o i),s):xs)) = [n] \/ o \/ ((fn s) \\ i) \/ (fn (Sum xs))

bn :: Species -> [Name]
bn Nil = []
bn (Def _ _) = []
bn (Par []) = []
bn (Par (x:xs)) = (bn x) \/ (bn (Par xs))
bn (New n s) = (bn s) \/ ((fn s) /\ (sites n))
bn (Sum []) = []
bn (Sum (((Tau _),s):xs)) = (bn s) \/ (bn (Sum xs))
bn (Sum (((Comm _ _ i),s):xs)) = (bn s) \/ ((fn s) /\ i) \/ (bn (Sum xs))

-- rename the given name in a species:
rename :: (Name,Name) -> Species -> Species
rename _ Nil = Nil
rename r (Def l ns) = Def l (vecRename r ns)
rename r (Par ss) = Par (map (rename r) ss)
rename r (New net s) = New (netRename r net) (rename r s)
rename r (Sum pfxs) = Sum (pfxRename r pfxs)

-- Renaming on name vectors
vecRename :: (Name,Name) -> [Name] -> [Name]
vecRename _ [] = []
vecRename r@(old,new) (n:ns)
    | n == old  = new : vecRename r ns
    | otherwise = n : vecRename r ns

-- Renaming on affinity networks:
netRename :: (Name,Name) -> AffNet -> AffNet
netRename rr (AffNet affss) = AffNet (netRename' rr affss)
    where
      netRename' _ [] = []
      netRename' r@(old,new) ((Aff ((n,n'),p)):affs)
          | n == old =  (Aff ((new,n'),p)):(netRename' r affs)
          | n' == old = (Aff ((n,new),p)):(netRename' r affs)
          | otherwise = (Aff ((n,n'),p)):(netRename' r affs)

-- Renaming on PrefixSpecies
pfxRename :: (Name,Name) -> [PrefixSpecies] -> [PrefixSpecies]
pfxRename _ [] = []
pfxRename r ((pfx,s):pfxs)
    = ((pfxRename' r pfx),(rename r s)):(pfxRename r pfxs)
      where
        pfxRename' _ (Tau rate) = Tau rate
        pfxRename' l@(old,new) (Comm n ons ins)
            | n == old  = Comm new (vecRename l ons) (vecRename l ins)
            | otherwise = Comm n (vecRename l ons) (vecRename l ins)

-- Substitution of free names in a Species:
-- sub [(n,n')] s = find free Names n in Species s
--                  and replace with New Names n'
-- Substitution is capture avoiding (will alpha-convert to avouid name capture)
sub :: [(Name,Name)] -> Species -> Species
sub [] s = s
sub ((old,new):rs) s
    -- name to be replaced is free and replacement is not bound
    -- then go ahead and substitute
    | (old `elem` (fn s)) && (not(new `elem` (bn s)))
        = sub rs (rename (old,new) s)
    -- name to be replaced is free, but replacement is bound
    -- then alpha-convert the bound name before substituting
    | (old `elem` (fn s)) && (new `elem` (bn s))
        = sub ((old,new):rs) (aconv (new,(renaming new s)) s)
    -- name to be replaced is not in the term, ignore
    | (not(old `elem` (fn s))) && (not(old `elem` (bn s)))
        = sub rs s
    | otherwise
    -- name to be replaced is not free -- error!
        = X.throw $ CpiException
          "CPi.Lib.sub: Tried to substitute a non-free name."

-- alpha-conversion of species
aconv :: (Name,Name) -> Species -> Species
aconv (old,new) s
    | (not(old `elem` (fn s))) && (not(new `elem` (fn s)))
        = rename (old,new) s
    | (new `elem` (fn s))
        = X.throw $ CpiException
          "CPi.Lib.aconv: Tried to alpha-convert to an existing free name."
    | otherwise
        = X.throw $ CpiException
          "CPi.Lib.aconv: Tried to alpha-convert a non-bound name."

-- a fresh renaming of a name in s
renaming :: Name -> Species -> Name
renaming nn ss = renaming' (renames nn) ss
    where
      renaming' (n:ns) s
          | not(n `elem` (fn s)) = n
          | otherwise            = renaming' ns s
      renaming' [] _
          = X.throw $ CpiException
            "CPi.Lib.renaming: Renaming stream has been exhausted."
      -- a stream of possible renamings for a given name
      renames x = [x++p | p <- iterate (++"'") "'"]

-- Fresh-for tests for restrictions
(#) :: AffNet -> Species -> Bool
net#s = ((sites net)/\(fn s)) == []

(##) :: AffNet -> AffNet -> Bool
net##net' = ((sites net)/\(sites net')) == []

-- Definition lookup:
lookupDef :: Env -> Species -> Maybe Species
lookupDef [] (Def _ _) = Nothing
lookupDef ((SpeciesDef i ps s):env) def@(Def x ns)
    | i == x    = Just (sub (zip ps ns) s)
    | otherwise = lookupDef env def
lookupDef ((ProcessDef _ _):env) def = lookupDef env def
lookupDef _ _ = X.throw $ CpiException
                "Unexpected pattern: CPi.Lib.lookupDef expects a Def!"

-- Reverse definition lookup:
revLookupDef :: Env -> Species -> Maybe Species
revLookupDef [] _ = Nothing
revLookupDef ((SpeciesDef i ps s):env) spec
    | nf spec == nf s  = Just (Def i ps)
    | otherwise        = revLookupDef env spec
revLookupDef ((ProcessDef _ _):env) spec = revLookupDef env spec

-- Process lookup by name:
lookupProcName :: Env -> String -> Maybe Process
lookupProcName [] _ = Nothing
lookupProcName ((SpeciesDef _ _ _):env) str = lookupProcName env str
lookupProcName ((ProcessDef name proc):env) str
    | (str == name) = Just proc
    | (otherwise)   = lookupProcName env str

-- Species lookup by name:
lookupSpecName :: Env -> String -> Maybe Species
lookupSpecName [] _ = Nothing
lookupSpecName ((ProcessDef _ _):env) str = lookupSpecName env str
lookupSpecName ((SpeciesDef name ns _):env) str
    | (str == name) = Just (Def name ns)
    | (otherwise)   = lookupSpecName env str

-- Process composition (p1,p2) -> p1||p2:
compose :: Process -> Process -> Process
compose (Process p1 a1) (Process p2 a2)
    = Process (compSpec p1 p2) (netUnion a1 a2)
      where
        compSpec s' ((s,c):ss)
            | lookup s s' == Nothing
                = compSpec ((s,c):s') ss
            | otherwise
                = compSpec (incSpec s c s') ss
        compSpec s' [] = s'
        incSpec s' c' ((s,c):ss)
            | s == s'
                = (s,(c + c')) : ss
            | otherwise
                = (s,c) : incSpec s' c' ss
        incSpec s' c' [] = [(s',c')]

speciesInProc :: Process -> [Species]
-- List of species in a process
speciesInProc (Process scs _) = map fst scs

-- A simplified string representation of a species
specName :: Species -> String
specName Nil = "0"
specName (Def n _) = n
specName (Par ss) = concat(L.intersperse "|" (map specName ss))
specName (Sum ps) = concat(L.intersperse "+" (map (specName . snd) ps))
specName (New _ s) = "(new)" ++ specName s

------------------
-- Normal form
------------------
-- The "normal form" here is reduction by structural congruence
-- (not including alpha equiv.) and alphanumeric ordering of term lists
-- This allows us to define a smaller equivalence based on the referential
-- equality (see above).
class Nf a where
    nf :: a -> a

-- normal form for species
instance Nf Species where
    nf m
        | result==m = result
        | otherwise = nf result
        where
          result = nf' m
          nf' Nil = Nil
          nf' (Def s ns) = Def s ns
          nf' (Sum []) = Nil
          -- commutativity and associativity of Sum
          nf' (Sum pfs) = Sum (L.sort (map nf pfs))
          nf' (Par []) = Nil
          nf' (Par [s]) = nf s
          -- commutativity and associativity of Par
          -- and 0|A = A
          nf' (Par ss) = Par (L.sort (dropNils (flatten (map nf ss))))
              where
                dropNils = filter (\x->x/=Nil)
                flatten [] = []
                flatten (x:xs) = (f x)++(flatten xs)
                    where
                      f (Par sss) = sss
                      f s = [s]
          -- (new M)(new N)A = (new MUN)A  when M#N and ¬(M#A or N#A)
          nf' (New net@(AffNet ns) (New net'@(AffNet ns') s))
              | (net##net') && not(net#s || net'#s)
                  = nf (New (net `netUnion` net') s)
              | net#s
                  = nf (New net' s)
              | net'#s
                  = nf (New net s)
              | otherwise
                  = (New (AffNet (L.sort ns)) (New (AffNet (L.sort ns')) (nf s)))
          -- (new M)(A|B) = A|(new M)B  when M#A
          nf' (New network (Par ss)) = liftfps network ss [] []
              where
                liftfps :: AffNet -> [Species] -> [Species] -> [Species] -> Species
                liftfps net [] [] ins
                    = New net (nf (Par ins))
                liftfps _ [] outs []
                    = nf (Par outs)
                liftfps net [] outs ins
                    = Par ((New net (nf (Par ins))):(map nf outs))
                liftfps net (s:sss) outs ins
                    | net#s     = liftfps net sss (s:outs) ins
                    | otherwise = liftfps net sss outs (s:ins)
          -- (new M)A = A  when M#A
          nf' (New net@(AffNet ns) s)
              | net#s     = nf s
              | otherwise = New (AffNet (L.sort ns)) (nf s)

instance Nf PrefixSpecies where
    nf (p,s) = (p,(nf s))
