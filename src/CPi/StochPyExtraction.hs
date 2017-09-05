module CPi.StochPyExtraction
  (stochPyModel, generateStochPy) where

import CPi.Symbolic
import CPi.Processes
import CPi.Vector
import CPi.AST (pretty)
import qualified CPi.AST as AST
import qualified Data.HashMap.Strict as H
import CPi.Simulation (Trace)
import Data.String.Utils
import CPi.ODEExtraction (sympyExpr, IVP(..), extractIVP)
-- , runPython)

-- import qualified Control.Exception as X
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

--import qualified Numeric.LinearAlgebra as LA

import System.IO.Unsafe
-- import qualified System.Process as OS
import System.Process (readProcess)
-- import Data.Maybe

stochPyModel :: IVP -> Either String String
stochPyModel ivp =
  if any isNothing exprs
  then Left "An ODE equation has an unbound variable"
  else Right $
  "Output_In_Conc: True\n" ++
  "Species_In_Conc: True\n\n" ++
  L.intercalate "\n" rules ++ "\n" ++
  L.intercalate "\n" initlines
    where
      species = ["S" ++ show i | i <- [(0::Integer)..]]
      varmap = M.fromList $ zip vars species
      IVP (vars,rhss,inits) = ivp
      mkgrowrule spec y =
        "grow" ++ spec ++ ":\n" ++
        "\t$pool > " ++ spec ++ "\n" ++
        "\t" ++ mkgrowrate y ++ "\n"
      mkgrowrate y = "abs(" ++ y ++ " + abs(" ++ y ++ "))/2"
      mkdecayrule spec y =
        "decay" ++ spec ++ ":\n" ++
        "\t" ++ spec ++ " > $pool\n" ++
        "\t" ++ mkdecayrate y ++ "\n"
      mkdecayrate y = "abs(-" ++ y ++ " + abs(" ++ y ++ "))/2"
      exprs = map (sympyExpr varmap) rhss
      growrules = zipWith mkgrowrule species (catMaybes exprs)
      decayrules = zipWith mkdecayrule species (catMaybes exprs)
      rules = zipWith (\x y -> x ++ "\n" ++ y) growrules decayrules
      initlines = [y ++ " = " ++ show y0 | (y,y0) <- zip species inits]


generateStochPy :: String -> AST.Env -> ConcreteAffinityNetwork -> P'  -> [Double] -> IO String
generateStochPy filename env network p inits = case stochPyModel (extractIVP env network p inits) of
  Right script -> do
    -- putStrLn $ "Python script:\n\n" ++ script
    writeFile filename script
    return script
  Left _ -> undefined
