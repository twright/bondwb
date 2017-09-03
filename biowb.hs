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

import qualified CPi.Lib as OldLib
import CPi.Plot
import CPi.AST
import CPi.Processes
import CPi.Simulation
import CPi.ParserNew (parseFile)
<<<<<<< Updated upstream
import CPi.ODEExtraction (solveODEPython)
=======
import CPi.ODEExtraction (solveODEPython, printODEPython, PrintStyle(..))
>>>>>>> Stashed changes

import System.Console.Haskeline
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import Graphics.Rendering.Chart.Renderable

import qualified Data.List as L
import qualified Control.Exception as X

import qualified Data.Map as M

import Debug.Trace(trace)


-- Some configurables:
welcome = "\nWelcome to the Biological Continuous Pi-calculus Workbench (BioWB).\n"
          ++"Type \"help\" for help.\n"
prompt = "BioWB:> "

-- Our environment will be a stack of the Haskeline,
-- State transformer (of CPi Definitions), and IO monads:
type Environment = InputT (StateT CPiModel IO)

-- Main function:
main :: IO ()
main = do putStrLn welcome;
          evalStateT (runInputT defaultSettings {historyFile = Just ".history"} loop) emptyCPiModel
              where
                loop :: Environment ()
                loop = do input <- getInputLine prompt
                          case input of
                            Nothing -> return ()
                            Just "" -> loop
                            Just "quit" -> return ()
                            Just i -> do doCommand i;
                                         loop
-- TODO: command autocomplete (see Haskeline docs).
--       can we use the command map?


doCommand :: String -> Environment ()
doCommand cmdln = let cmd = head $ words cmdln in
                  case lookup cmd commands of
                    Nothing -> say "Try again."
                    Just x  -> cmdFn x cmdln

---------------
-- Command map:
---------------

-- TODO: document how to add new commands

data CmdRec = CmdRec {cmdFn::String->Environment (),
                      cmdHelp::(String,String)}

commands :: [(String,CmdRec)]
commands = [("help",
             CmdRec {cmdFn = helpCmd,
                     cmdHelp = helpTextHelp}),
            ("quit",
             CmdRec {cmdFn = undefined,
                     cmdHelp = helpTextQuit}),
            ("load",
             CmdRec {cmdFn = loadCmd,
                     cmdHelp = helpTextLoad}),
            ("env",
             CmdRec {cmdFn = envCmd,
                     cmdHelp = helpTextEnv}),
            ("clear",
             CmdRec {cmdFn = clearCmd,
                     cmdHelp = helpTextClear}),
            ("trans",
             CmdRec {cmdFn = transCmd,
                     cmdHelp = helpTextTrans}),
            ("plot",
             CmdRec {cmdFn = plotPythonCmd,
                     cmdHelp = helpTextPlot}),
<<<<<<< Updated upstream
=======
            ("odes",
             CmdRec {cmdFn = extractODECmd,
                     cmdHelp = helpTextPlot}),
>>>>>>> Stashed changes
            ("plotn",
             CmdRec {cmdFn = plotCmd,
                     cmdHelp = helpTextPlot}),
            ("plotUptoEpsilon",
             CmdRec {cmdFn = plotEpsilonCmd,
                     cmdHelp = helpTextPlot})
                     ]

---------------------
-- Command Functions:
---------------------

plotCmd :: String -> Environment ()
plotCmd x = do
  abstractModel <- getEnv
  let args    = words x
  let name    = args!!1
      start   = read(args!!2) :: Double
      end     = read(args!!3) :: Double
      tolabs  = read(args!!4) :: Double
      tolrel  = read(args!!5) :: Double
      h       = read(args!!6)
      hmin    = read(args!!7)
      hmax    = read(args!!8)
      -- h       = (end - start) / fromIntegral steps
      n       = read(args!!9) :: Int
  case concretifyModel abstractModel of
    Right (env, defs) ->
      case M.lookup name defs of
        Just (network, p) -> do
          let simulator = simulateMaxSpecies n env network tolabs tolrel h hmin hmax start p
              res       = takeWhile ((<=end).fst) simulator
          lift $ lift $ plotTrace res
        Nothing -> say $ "Process " ++ name ++ " not defined!"
    Left err -> say $ "Error in model: " ++ err

plotPythonCmd :: String -> Environment ()
plotPythonCmd x = do
  abstractModel <- getEnv
  let args    = words x
  let name    = args!!1
      start   = read(args!!2) :: Double
      end     = read(args!!3) :: Double
      n       = read(args!!4) :: Int
  case symbolifyModel abstractModel of
    Right (env, defs) ->
      case M.lookup name defs of
        Just (network, p, inits) -> do
          let res = solveODEPython env network p inits (n, (start, end))
          lift $ lift $ plotTrace res
        Nothing -> say $ "Process " ++ name ++ " not defined!"
    Left err -> say $ "Error in model: " ++ err

<<<<<<< Updated upstream
=======
extractODECmd :: String -> Environment ()
extractODECmd x = do
  abstractModel <- getEnv
  let args    = words x
  let name    = args!!1
  case symbolifyModel abstractModel of
    Right (env, defs) ->
      case M.lookup name defs of
        Just (network, p, _) -> do
          let res = printODEPython env network p Pretty
          say res
        Nothing -> say $ "Process " ++ name ++ " not defined!"
    Left err -> say $ "Error in model: " ++ err

>>>>>>> Stashed changes
plotEpsilonCmd :: String -> Environment ()
plotEpsilonCmd x = do
  abstractModel <- getEnv
  let args    = words x
  let name    = args!!1
      start   = read(args!!2) :: Double
      end     = read(args!!3) :: Double
      tolabs  = read(args!!4) :: Double
      tolrel  = read(args!!5) :: Double
      h       = read(args!!6)
      hmin    = read(args!!7)
      hmax    = read(args!!8)
      -- h       = (end - start) / fromIntegral steps
      epsilon = read(args!!9) :: Double
  case concretifyModel abstractModel of
    Right (env, defs) ->
      case M.lookup name defs of
        Just (network, p) -> do
          let simulator = simulateUptoEpsilon epsilon env network tolabs tolrel h hmin hmax start p
              res       = takeWhile ((<=end).fst) simulator
          lift $ lift $ plotTrace res
        Nothing -> say $ "Process " ++ name ++ " not defined!"
    Left err -> say $ "Error in model: " ++ err

-- help Command
helpCmd :: String -> Environment ()
helpCmd x
    | not(null(param x))
        = case lookup (param x) commands of
            Nothing -> say $ "Sorry no help on \""++x++"\"."
            Just r -> let (c,d) = cmdHelp r in
                      say $ "\n"++c++"\n\t"++d++"\n"
    | otherwise
        = say $ "\nThe available commands are:\n"
          ++"\n" ++ OldLib.prettyList (map fst commands) ++ "\n\n"
          ++"Type \"help <command>\" for help on a specific command.\n"

-- load Command
loadCmd :: String -> Environment ()
loadCmd x = do say $ "Loading: " ++ param x
               f <- getFile (param x)
               case parseFile x f of
                 Left err -> say $ "Parse error:\n" ++ show err
                 Right ds -> do putEnv ds;
                                say "Done. Type \"env\" to view."

-- env Command
envCmd :: String -> Environment ()
envCmd _ = do s <- getEnv;
              say undefined

-- clear Command
clearCmd :: String -> Environment ()
clearCmd _ = putEnv emptyCPiModel

-- trans Command
transCmd :: String -> Environment ()
transCmd x = undefined

----------------------
-- Command help texts:
----------------------

helpTextHelp = ("help <command>","Shows help on a specific command.")
helpTextQuit = ("quit","Quits the session, same as Ctrl+D")
helpTextLoad = ("load <filename>","Loads a CPi definition file.")
helpTextEnv = ("env","Shows the contents of the current environment.")
helpTextSpecies = ("species <definition>","Adds a species definition to the "
                   ++"current environment.")
helpTextClear = ("clear","Clears the environment.")
helpTextProcess = ("process <definition>","Adds a process definition to the "
                   ++"environment.")
helpTextTrans = ("trans <process>","Shows the transitions of a process.")
helpTextOdes = ("odes <process>","Shoes the ODEs for a process.")
helpTextPlot = ("plot <process> <start> <end> <points>","Plots the time series of a process for the given interval [start,end] with the given number of time points.")

---------------------
-- Utility functions:
---------------------

-- Say something to the user:
say = outputStrLn

-- Get the Environment state:
getEnv :: Environment CPiModel
getEnv = lift get

-- Write the Environment state:
putEnv :: CPiModel -> Environment ()
putEnv = lift . put

-- Add to the Environment state:
-- addEnv :: OldLib.Definition -> Environment ()
-- addEnv x = do env <- getEnv;
--               putEnv (x:env)

-- Read in a file:
getFile :: FilePath -> Environment String
getFile = lift . lift . readFile

-- Write a file:
putFile :: FilePath -> String -> Environment ()
putFile f s = lift $ lift $ writeFile f s

-- get the parameters from a command line:
params :: String -> [String]
params cmdln = tail(words cmdln)
-- just the first:
param :: String -> String
param cmdln = let ps = params cmdln in
              case ps of
                []     -> []
                (p:ps) -> p
