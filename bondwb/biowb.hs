-- (C) Copyright Thomas Wright 2016-2019, Chris Banks 2011-2012

-- This file is part of The Bond Calculus Workbench (BondWB).

-- BondWB is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- BondWB is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with BondWB.  If not, see <http://www.gnu.org/licenses/>.

import BioCalcLib.Lib
import BondCalculus.Plot
import BondCalculus.AST
import BondCalculus.Processes
import BondCalculus.Simulation
import BondCalculus.ParserNew (parseFile)
import BondCalculus.ODEExtraction (solveODEPython, printODEPython, PrintStyle(..))
import BondCalculus.StochPyExtraction (generateStochPy, simulateStochPy)
import BondCalculus.ArgListParser (args)

import System.Console.Haskeline hiding (defaultPrefs)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Text.Megaparsec (parse)

import Options.Applicative
import Data.Semigroup ((<>))

import qualified Data.Map as M


-- Some configurables:
welcome = "\nWelcome to the Bond Calculus Workbench (BondWB).\n"
          ++"Type \"help\" for help.\n"
prompt = "BondWB:> "


-- Our environment will be a stack of the Haskeline,
-- State transformer (of CPi Definitions), and IO monads:
type Environment = InputT (StateT BondCalculusModel IO)


-- Data structures for command arguments
data Command = PlotArgs String Double Double Double Double Double Double Double Int
             | PlotPythonArgs String Double Double Int
             | LoadArgs String
             | SaveStochPyArgs String Double String
             | PlotStochPyArgs String Double Int String
             | PlotUptoEpsilonArgs String Double Double Double Double Double Double Double Double
             | ODEsPrettyArgs String
             | ODEsLaTeXArgs String
             | EnvArgs
             | ClearArgs
             deriving (Show)


-- Definition of parsers for commands
commands :: [(String, Parser Command, String)]
commands = [
    ( "plot"
    , PlotPythonArgs
        <$> argument str  (metavar "process")
        <*> argument auto (metavar "start")
        <*> argument auto (metavar "end")
        <*> argument auto (metavar "n")
    , "Plot a graph via an extracted python script." ),
    ( "plotn"
    , PlotArgs
        <$> argument str  (metavar "process")
        <*> argument auto (metavar "start")
        <*> argument auto (metavar "end")
        <*> argument auto (metavar "tolabs")
        <*> argument auto (metavar "tolrel")
        <*> argument auto (metavar "h")
        <*> argument auto (metavar "hmin")
        <*> argument auto (metavar "hmax")
        <*> argument auto (metavar "n")
    , "Plot a graph directly using semantics and integrated " ++
        "numerical methods, for upto n species." ),
    ( "plotepsilon"
    , PlotUptoEpsilonArgs
        <$> argument str  (metavar "process")
        <*> argument auto (metavar "start")
        <*> argument auto (metavar "end")
        <*> argument auto (metavar "tolabs")
        <*> argument auto (metavar "tolrel")
        <*> argument auto (metavar "h")
        <*> argument auto (metavar "hmin")
        <*> argument auto (metavar "hmax")
        <*> argument auto (metavar "epsilon")
    , "Plot a graph directly using semantics and integrated " ++
      "numerical methods, with concentration threshold epsilon." ),
    ( "load"
    , LoadArgs
        <$> argument str (metavar "file")
    , "Load a model file." ),
    ( "env"
    , pure EnvArgs
    , "Display the current environment." ),
    ( "clear"
    , pure ClearArgs
    , "Clear the current environment." ),
    ( "savestochpy"
    , SaveStochPyArgs
        <$> argument str  (metavar "process")
        <*> argument auto (metavar "step")
        <*> argument str (metavar "filename")
    , "Save StochPy Stochastic Simulation model." ),
    ( "plotstochpy"
    , PlotStochPyArgs 
        <$> argument str  (metavar "process")
        <*> argument auto (metavar "step")
        <*> argument auto (metavar "n")
        <*> argument str  (metavar "method")
    , "Plot Stochastic simulation results." ),
    ( "odes"
    , ODEsPrettyArgs
        <$> argument str (metavar "process")
    , "Display ODEs for process." ),
    ( "odeslatex"
    , ODEsLaTeXArgs
        <$> argument str (metavar "process")
    , "Display latex formatted ODEs for process.") ]


---------------------
-- Command Functions:
---------------------

cmd :: Command -> Environment ()
-- load bondwb file
cmd (LoadArgs filename) = do
    say $ "Loading: " ++ filename
    f <- getFile filename
    case parseFile filename f of
       Left err -> say $ "Parse error:\n" ++ show err
       Right ds -> do putEnv ds;
                      say "Done. Type \"env\" to view."
-- Display the current environment
cmd (ClearArgs) = putEnv emptyBondCalculusModel
-- Clear the current environment
cmd (EnvArgs) = undefined
-- plot ODE trace manually, with max n species
cmd (PlotArgs name start end tolabs tolrel h hmin hmax n) =
    applyConcrete name $ \env (network, _, p) -> 
        let simulator = simulateMaxSpecies n env network tolabs tolrel h hmin hmax start p
        in plotTrace $ takeWhile ((<=end).fst) simulator
-- plot ODE trace via Python script extraction
cmd (PlotPythonArgs name start end n) =
    applySymbolic name $ \env (network, _, p, inits) -> 
        plotTrace $ solveODEPython env network p inits (n, (start, end))
-- plot ODE trace manually, truncating species with concentration <= n
cmd (PlotUptoEpsilonArgs name start end tolabs tolrel h hmin hmax epsilon) =
    applyConcrete name $ \env (network, _, p) ->
        let simulator = simulateUptoEpsilon epsilon env network tolabs tolrel h hmin hmax start p
        in plotTrace $ takeWhile ((<=end).fst) simulator
-- plot Guillespie SSA traces via StochPy
cmd (PlotStochPyArgs name step n method) =
    applySymbolic name $ \env (network, _, p, inits) -> do
        res <- simulateStochPy method n env network p step inits
        plotTrace res
-- save StochPy model
cmd (SaveStochPyArgs name step filename) =
    applySymbolic name $ \env (network, _, p, inits) ->
        generateStochPy filename env network p step inits >> return ()
-- Extract ODEs
cmd (ODEsPrettyArgs name) = 
    applySymbolic name $ \env (network, _, p, _) ->
        putStrLn $ printODEPython env network p Pretty
-- Extract LaTeX formatted ODEs
cmd (ODEsLaTeXArgs name) =
    applySymbolic name $ \env (network, _, p, _) ->
        putStrLn $ printODEPython env network p LaTeX


-- Main function:
main :: IO ()
main = do putStrLn welcome;
          evalStateT (runInputT defaultSettings {historyFile = Just ".history"} loop) emptyBondCalculusModel
              where
                loop :: Environment ()
                loop = do input <- getInputLine prompt
                          case input of
                            Nothing -> return ()
                            Just "" -> loop
                            Just "quit" -> return ()
                            Just i -> do 
                                x <- doCommandParse i
                                mapM cmd x
                                loop

doCommandParse :: String -> Environment (Maybe Command)
doCommandParse cmdln = case parse args "" cmdln of 
    Left err -> do
        say $ "Arg parse error: " ++ show err
        return Nothing
    Right cmds ->
        case execParserPure defaultPrefs commandParserInfo cmds of
            Success x -> return $ Just x
            Failure err -> do
                let (msg, exit) = renderFailure err "bondwb"
                say $ "Invalid arguments: " ++ msg
                return $ Nothing

-- Create command parsers
commandParser :: Parser Command
commandParser = hsubparser $ foldl1 (<>)
                             [ (command c $ info p $ progDesc h)
                             | (c, p, h) <- commands ]

commandParserInfo :: ParserInfo Command
commandParserInfo = info commandParser mempty


---------------------
-- Utility functions:
---------------------

-- Say something to the user:
say = outputStrLn

-- Get the Environment state:
getEnv :: Environment BondCalculusModel
getEnv = lift get

-- Write the Environment state:
putEnv :: BondCalculusModel -> Environment ()
putEnv = lift . put

-- Read in a file:
getFile :: FilePath -> Environment String
getFile = lift . lift . readFile

-- Write a file:
putFile :: FilePath -> String -> Environment ()
putFile f s = lift $ lift $ writeFile f s

applySymbolic :: String -> (Env -> SymbolicDef -> IO()) -> Environment ()
applySymbolic = applyToEnv symbolifyModel

applyConcrete :: String -> (Env -> ConcreteDef -> IO()) -> Environment ()
applyConcrete = applyToEnv concretifyModel

applyToEnv :: (BondCalculusModel -> Either String (Env, M.Map String b)) -> String -> (Env -> b -> IO()) -> Environment()
applyToEnv g name f = do
    abstractModel <- getEnv
    case g abstractModel of
        Right (env, defs) -> 
            case M.lookup name defs of
                Just def -> lift $ lift $ f env def
                Nothing -> say $ "Process " ++ name ++ " not defined!"
        Left err -> say $ "Error in model: " ++ err
