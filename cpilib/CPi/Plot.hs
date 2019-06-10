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

module CPi.Plot
    (plotTimeSeries,
     plotTimeSeriesD,
     plotTimeSeriesFiltered,
     plotTimeSeriesFilteredD,
     plotTimeSeriesToFile,
     plotTimeSeriesToFileFiltered,
     phasePlot2,
     phasePlot2ToFile,
     phasePlot2D
    ) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Colour
-- import Data.Accessor
import Data.Default.Class
import Control.Lens
import qualified Control.Exception as X
import qualified Numeric.LinearAlgebra as LA
import Control.Monad
import Graphics.UI.Gtk.Misc.DrawingArea
import qualified Data.List as L

import BioCalcLib.Plot
import BioCalcLib.Lib
import CPi.Lib

-- Takes data from the ODE solver and plots them
plotTimeSeries :: LA.Vector Double -> LA.Matrix Double -> [Species] -> IO ()
plotTimeSeries ts soln ss
    = plot
      (LA.toList ts)
      (zip (map pretty ss) (map LA.toList (LA.toColumns soln)))

plotTimeSeriesD :: DrawingArea -> LA.Vector Double -> LA.Matrix Double -> [Species] -> IO ()
plotTimeSeriesD da ts soln ss
    = plotD
      da
      (LA.toList ts)
      (zip (map pretty ss) (map LA.toList (LA.toColumns soln)))

-- Plots the data to a PDF file
plotTimeSeriesToFile :: LA.Vector Double -> LA.Matrix Double -> [Species] -> String -> IO ()
plotTimeSeriesToFile ts soln ss
    = plotToFile
      (LA.toList ts)
      (zip (map pretty ss) (map LA.toList (LA.toColumns soln)))

-- Only plots selected species
plotTimeSeriesFiltered :: LA.Vector Double -> LA.Matrix Double -> [Species] -> [Species]
                       -> IO ()
plotTimeSeriesFiltered ts soln ss ss'
    = plot
      (LA.toList ts)
      (filter (\(s,_)-> s `elem` map specName ss')
       (zip (map specName ss) (map LA.toList (LA.toColumns soln))))

plotTimeSeriesFilteredD :: DrawingArea -> LA.Vector Double -> LA.Matrix Double -> [Species] -> [Species] -> IO ()
plotTimeSeriesFilteredD drawingArea ts soln ss ss'
    = plotD
      drawingArea
      (LA.toList ts)
      (filter (\(s,_)-> s `elem` map specName ss')
       (zip (map specName ss) (map LA.toList (LA.toColumns soln))))

-- Only plots selected species to a PDF file
plotTimeSeriesToFileFiltered :: LA.Vector Double -> LA.Matrix Double -> [Species] -> [Species]
                             -> String -> IO ()
plotTimeSeriesToFileFiltered ts soln ss ss'
    = plotToFile
      (LA.toList ts)
      (filter (\(s,_)-> s `elem` map specName ss')
       (zip (map pretty ss) (map LA.toList (LA.toColumns soln))))

---------------
-- Phase plots:
---------------

-- a plot of two dimensions:
phasePlot2 :: LA.Vector Double
           -> LA.Matrix Double
           -> [Species]
           -> (Species,Species)
           -> IO ()
phasePlot2 ts soln ss ss' = plotPhase $ filterPhasePlot ts soln ss ss'

filterPhasePlot :: LA.Vector Double
                -> LA.Matrix Double
                -> [Species]
                -> (Species,Species)
                -> [(String, [Double])]
filterPhasePlot _ soln ss ss'
    = filter (\(s,_) -> s == specName (fst ss') || s == specName (snd ss'))
             (zip (map specName ss) (map LA.toList $ LA.toColumns soln))

phasePlot2ToFile :: LA.Vector Double
                 -> LA.Matrix Double
                 -> [Species]
                 -> (Species, Species)
                 -> String
                 -> IO ()
phasePlot2ToFile ts soln ss ss'
    = plotPhaseToFile (filterPhasePlot ts soln ss ss')

phasePlot2D :: DrawingArea
            -> LA.Vector Double
            -> LA.Matrix Double
            -> [Species]
            -> (Species,Species)
            -> IO ()
phasePlot2D da ts soln ss ss' = plotPhaseD da $ filterPhasePlot ts soln ss ss'

