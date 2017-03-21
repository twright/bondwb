-- (C) Copyright Chris Banks 2011-2012

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
import Graphics.Rendering.Chart.Layout

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
plotTimeSeriesToFile ts soln ss file
    = plotToFile
      (LA.toList ts)
      (zip (map pretty ss) (map LA.toList (LA.toColumns soln)))
      file

-- Only plots selected species
plotTimeSeriesFiltered :: LA.Vector Double -> LA.Matrix Double -> [Species] -> [Species]
                       -> IO ()
plotTimeSeriesFiltered ts soln ss ss'
    = plot
      (LA.toList ts)
      (filter (\(s,_)-> s `elem` (map specName ss'))
       (zip (map specName ss) (map LA.toList (LA.toColumns soln))))

plotTimeSeriesFilteredD :: DrawingArea -> LA.Vector Double -> LA.Matrix Double -> [Species] -> [Species] -> IO ()
plotTimeSeriesFilteredD drawingArea ts soln ss ss'
    = plotD
      drawingArea
      (LA.toList ts)
      (filter (\(s,_)-> s `elem` (map specName ss'))
       (zip (map specName ss) (map LA.toList (LA.toColumns soln))))

-- Only plots selected species to a PDF file
plotTimeSeriesToFileFiltered :: LA.Vector Double -> LA.Matrix Double -> [Species] -> [Species]
                             -> String -> IO ()
plotTimeSeriesToFileFiltered ts soln ss ss' file
    = plotToFile
      (LA.toList ts)
      (filter (\(s,_)-> s `elem` (map specName ss'))
       (zip (map pretty ss) (map LA.toList (LA.toColumns soln))))
      file

-- Plots the time series in a GTK window
plot :: [Double] -> [(String,[Double])] -> IO ()
plot ts dims = renderableToWindow (toRenderable (layout ts dims)) 639 480

plotD  :: DrawingArea -> [Double] -> [(String,[Double])] -> IO ()
plotD da ts dims = do
  updateCanvas (toRenderable (layout ts dims)) da
  return ()

-- Plots the time series to a file
plotToFile :: [Double] -> [(String,[Double])] -> String -> IO ()
plotToFile ts dims file = void $ renderableToFile (FileOptions (842, 595) PDF) file (toRenderable (layout ts dims))

-- gets a plot layout with plots for each dimension
layout ts dims = layout_plots .~ plots ts (colours (length dims)) dims
  $ def
                 -- layout1_legend ^= Nothing $ {-remove to add legend-}

-- gets the plots for each dimension
-- plots :: [Double] -> [AlphaColour Double] -> [(String,[Double])] ->
--          [Either (Plot Double Double) b]
plots :: [Double] -> [AlphaColour Double] -> [(String,[Double])] ->
         [Plot Double Double]
plots _ _ [] = []
plots ts (colour:cs) ((lbl,pts):dims)
    = (toPlot
      $ plot_lines_style .~ solidLine 1 colour
      $ plot_lines_values .~ [zip ts pts]
      $ plot_lines_title .~ lbl
      $ def
      ) : plots ts cs dims
plots _ [] _ = X.throw $ CpiException
               "CPi.Plot.plots: Run out of colours!"

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
filterPhasePlot ts soln ss ss'
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

plotPhaseToFile :: [(String, [Double])] -> String -> IO ()
plotPhaseToFile dims file = void $ renderableToFile
                                   (FileOptions (842, 595) PDF) file
                                   (toRenderable (layout2phase dims))

-- getRender :: LA.Vector Double -> LA.Matrix Double -> [Species] -> [Species] -> Int -> Int -> (Maybe (Render ()))
-- getRender  ts soln ss ss' ww wh = (Just $ runBackend (defaultEnv bitmapAlignmentFns)
-- 						( void $ render (getLayout ts soln ss ss') (fromIntegral ww, fromIntegral wh) )
-- 					)

plotPhase dims = renderableToWindow (toRenderable (layout2phase dims)) 640 480

plotPhaseD  :: DrawingArea -> [(String,[Double])] -> IO ()
plotPhaseD da dims = void $ updateCanvas (toRenderable (layout2phase dims)) da

phasePlot2D :: DrawingArea
            -> LA.Vector Double
            -> LA.Matrix Double
            -> [Species]
            -> (Species,Species)
            -> IO ()
phasePlot2D da ts soln ss ss' = plotPhaseD da $ filterPhasePlot ts soln ss ss'

plotphase pts
    = toPlot
      $ plot_lines_values .~ [pts]
      $ plot_lines_style .~ solidLine 1 (opaque blue)
      $ def

layout2phase dims
    = layout_plots .~ [plotphase $ zip (snd (dims!!0)) (snd (dims!!1))]
--      $ layout1_bottom_axis ^: laxis_generate ^= autoScaledLogAxis defaultLogAxis
      $ layout_x_axis . laxis_title .~ "["++fst (dims!!0)++"]"
--      $ layout1_left_axis ^: laxis_generate ^= autoScaledLogAxis defaultLogAxis
      $ layout_y_axis . laxis_title .~ "["++fst (dims!!1)++"]"
      $ def


-------------------

-- gives n visually distinct colours
-- algorithm taken from the MATLAB 'varycolor' function
-- by Daniel Helmick: http://j.mp/xowLV2
colours :: Int -> [AlphaColour Double]
colours n
    | n<=0 = []
    | n==1 = [clr 0 1 0]
    | n==2 = [clr 0 1 0,clr 0 1 1]
    | n==3 = [clr 0 1 0,clr 0 1 1,clr 0 0 1]
    | n==4 = [clr 0 1 0,clr 0 1 1,clr 0 0 1,clr 1 0 1]
    | n==5 = [clr 0 1 0,clr 0 1 1,clr 0 0 1,clr 1 0 1,clr 1 0 0]
    | n==6 = [clr 0 1 0,clr 0 1 1,clr 0 0 1,clr 1 0 1,clr 1 0 0,clr 0 0 0]
    | otherwise = sec 1 ++ sec 2 ++ sec 3 ++ sec 4 ++ sec 5
    where
      s = fromIntegral(n `div` 5)
      e = fromIntegral(n `mod` 5)
      f x y
          | x<=y = 1.0
          | otherwise = 0.0
      g x = [1..(s+(f x e))]
      sec x
          | x==1 = [clr 0 1 ((m-1)/(s+(f x e)-1)) | m<-g x]
          | x==2 = [clr 0 ((s+(f x e)-m)/(s+(f x e))) 1 | m<-[1..(s+(f x e))]]
          | x==3 = [clr (m/(s+(f x e))) 0 1 | m<-[1..(s+(f x e))]]
          | x==4 = [clr 1 0 ((s+(f x e)-m)/(s+(f x e))) | m<-[1..(s+(f x e))]]
          | x==5 = [clr ((s+(f x e)-m)/(s+(f x e))) 0 0 | m<-[1..(s+(f x e))]]
          | otherwise = undefined

clr :: Double -> Double -> Double -> AlphaColour Double
clr r g b = opaque(sRGB r g b)


{-- test data
testT = [0.0,0.1..2500.0]::[Double]
testD1 = [0,0.1..2500]::[Double]
testD2 = [x*x|x<-[0,0.1..2500]]::[Double]

testPlot1 = plot_lines_style ^= solidLine 1 (opaque $ sRGB 0.5 0.5 1)
            $ plot_lines_values ^= [zip testT testD1]
            $ plot_lines_title ^= "test1"
            $ defaultPlotLines

testPlot2 = plot_lines_style ^= solidLine 1 (opaque red)
            $ plot_lines_values ^= [zip testT testD2]
            $ plot_lines_title ^= "test2"
            $ defaultPlotLines

testLayout = layout1_title ^= "Test graph!"
             $ layout1_plots ^= [Left (toPlot testPlot1),
                                 Left (toPlot testPlot2)]
             $ defaultLayout1

testPlot = renderableToWindow (toRenderable testLayout) 640 480
-}
