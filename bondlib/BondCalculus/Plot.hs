-- (C) Copyright Chris Banks 2011-2012

-- This file is part of The Continuous Pi-calculus Workbench (BondCalculusWB).

--     BondCalculusWB is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.

--     BondCalculusWB is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.

--     You should have received a copy of the GNU General Public License
--     along with BondCalculusWB.  If not, see <http://www.gnu.org/licenses/>.


module BondCalculus.Plot (plotTrace) where

import qualified Data.List as L

import BioCalcLib.Plot (plot)
import BondCalculus.Simulation (Trace, formatTrace)

plotTrace :: Trace -> IO ()
plotTrace tr = plot ts specConcs
  where tr' = formatTrace tr
        ts = map fst tr'
        concs = map snd tr'
        specs = L.nub $ L.sort $ concatMap (map fst) concs
        specConcs = [(spec, [sum [c | (s,c) <- sc, s==spec] | sc <- concs])
                    | spec <- specs]
