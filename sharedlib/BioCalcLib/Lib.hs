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
module BioCalcLib.Lib
    (Pretty,
     pretty,
     prettys,
     prettyList,
     card,
     tri1,
     tri2,
     tri3,
     s2d,
     d2s,
     remove,
     replace,
     ifnotnil,
     infty,
     (/\),(\/),(\\),
     CpiException(..)
     ) where

import qualified Data.List as L
--import qualified Data.Map as Map
--import Data.Map (Map)
import qualified Control.Exception as X
import qualified Data.Typeable as T

newtype CpiException = CpiException String
                    deriving (Show,T.Typeable)
instance X.Exception CpiException

infty = 1/0

----------------------
-- Pretty printing:
----------------------
-- Instances of Pretty class are pretty-printable
-- CPi components.
class (Show a) => Pretty a where
    pretty :: a -> String
    pretty = show

---------------------
-- Utility functions:
---------------------

--Set operations:
-- | Set union.
(\/) :: (Eq a) => [a] -> [a] -> [a]
(\/) = L.union

-- | Set intersection.
(/\) :: (Eq a) => [a] -> [a] -> [a]
(/\) = L.intersect

-- | Set difference.
(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) = (L.\\)

--Real<->String
d2s :: Double -> String
d2s x = show x

s2d :: String -> Double
s2d x = read x :: Double

-- | If List is not nil then apply Function else return Default.
ifnotnil :: [a]        -- ^ List
         -> ([a] -> b) -- ^ Function
         -> b          -- ^ Default
         -> b
ifnotnil [] _ b = b
ifnotnil xs f _ = f xs

-- | Pretty print a list of pretty printable expressions.
prettys :: (Pretty a) => [a] -> String
prettys x = concat $ map (\z->(pretty z)++"\n") x

-- | Pretty print a list
prettyList :: [String] -> String
prettyList x = L.concat $ L.intersperse "\n" x

-- | Replace first matched element of a list with something else.
replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace src dst (x:xs)
    | x==src    = dst:xs
    | otherwise = x:(replace src dst xs)

-- | Remove first matched element of a list.
remove :: (Eq x) => x -> [x] -> [x]
remove _ [] = []
remove m (x:xs)
    | x==m      = xs
    | otherwise = x:(remove m xs)

-- | Count of an element in a list.
card :: (Eq a) => a -> [a] -> Integer
card e l = toInteger $ length $ filter (\a->a==e) l

-- | First element of a triple.
tri1 :: (x, y, z) -> x
tri1 (x,_,_) = x
-- | Second element of a triple.
tri2 :: (x, y, z) -> y
tri2 (_,x,_) = x
-- | Third element of a triple.
tri3 :: (x, y, z) -> z
tri3 (_,_,x) = x
