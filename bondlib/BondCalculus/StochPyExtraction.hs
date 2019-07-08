module BondCalculus.StochPyExtraction
  (stochPyModel, generateStochPy, reaction, ReactionVect, reactions, ReactionSystem(..), extractReactionSystem, fromIVPToStochPyModel, simulateStochPy) where

import BondCalculus.Symbolic
import BondCalculus.Vector
import BondCalculus.AST
import BondCalculus.Base
import qualified BondCalculus.AST as AST
import BondCalculus.Transitions (MTS)
import Data.Bifunctor
import BondCalculus.Simulation (Trace)
-- import qualified Data.HashMap.Strict as H
-- import BondCalculus.Simulation (Trace)
-- import Data.String.Utils
import BondCalculus.ODEExtraction (sympyExpr, IVP(..), sympySimplify, runPython)
-- , runPython)

-- import qualified Control.Exception as X
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import BondCalculus.Processes

--import qualified Numeric.LinearAlgebra as LA

-- import System.IO.Unsafe
-- import qualified System.Process as OS
-- import System.Process (readProcess)
-- import Data.Maybe

type ReactionVect k = Vect (Tensor (ProcessVect Conc) (ProcessVect Conc)) k

reaction :: (Vector k (DirectionVect k), Vector k (ReactionVect k), Nullable (DirectionVect k), Expression k) =>
            [DirectionVect k] -> ReactionVect k
reaction = multilinear react' <$> filter (/=vectZero)
  where react' xs = vect( sourceVect xs :* targetVect xs )
        source (spec :* _) = spec
        targetVect xs = embed $ concretify $ foldl (<|>) (mkAbsBase Nil) (map target xs)
        target (_ :* spec') = spec'
        sourceVect xs = foldl (+>) vectZero (map (embed.source) xs)

reactions :: (Vector k (ProcessVect k), Show k, DoubleExpression k, ExpressionOver Double k) => (Species -> MTS) -> ConcreteAffinityNetwork Double -> ProcessVect k -> ReactionVect k
reactions tr network p = simplify $ dPdt'' reaction tr network p

type ConcVect = Vect String Conc
newtype ReactionSystem = ReactionSystem ([String], [(SymbolicExpr Double, ConcVect, ConcVect)], [Double])
  deriving (Show, Eq, Ord)

extractReactionSystem :: AST.Env -> ConcreteAffinityNetwork Double -> P' Double -> [Double] -> ReactionSystem
extractReactionSystem env network p inits = ReactionSystem (vars, transitions, inits)
  where tr = tracesGivenNetwork network env
        vars = map (pretty.simplify.snd) (toList p)
        reacts = reactions tr network p
        transitions = [ (simplify coeff, toConcVect u, toConcVect v)
                      | (coeff, u :* v) <- toList reacts]
        toConcVect  = fromList . map (second (pretty.simplify)) . toList

fromIVPToStochPyModel :: IVP Double -> Either String String
fromIVPToStochPyModel ivp =
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

stochPyModel :: ReactionSystem -> Double -> Either String String
stochPyModel (ReactionSystem (vars, transitions, inits)) h =
  if any isNothing transitions'
  then Left "An ODE equation has an unbound variable"
  else Right $
  "Output_In_Conc: True\n" ++
  "Species_In_Conc: True\n\n" ++
  L.intercalate "\n" rules ++ "\n" ++
  "h = " ++ show h ++ "\n" ++
  L.intercalate "\n" initlines
    where
      species = ["S" ++ show i | i <- [(0::Integer)..]]
      reacts = ["R" ++ show i | i <- [(0::Integer)..]]
      varmap = M.fromList $ zip vars species
      varmaph = fmap (\s -> "(" ++ s ++ "*h)") varmap
      rules = [
            i ++ ":\n" ++
            "\t" ++ formatVector u ++ " > " ++ formatVector v ++ "\n" ++
            "\t" ++ rate ++ "\n"
          | (i, (rate, u, v)) <- reacts `zip` catMaybes transitions'
        ]
      transitions' = fmap (\(rate,u,v) -> do
                      rate' <- sympyExpr varmaph rate
                      u' <- vectorVarLookup u
                      v' <- vectorVarLookup v
                      return (sympySimplify ("(" ++ rate' ++ ")/h"),u',v')) transitions
      vectorVarLookup u =
        let us = map (second (`M.lookup` varmap)) $ toList u
        in if any (isNothing.snd) us
          then Nothing
          else Just (fromList [(k,v) | (k, Just v) <- us])
      formatVector u = if null us
                       then "$pool"
                       else L.intercalate " + "
                              ["{" ++ show k ++ "} " ++ i | (k,i) <- us]
        where us = toList u
      initlines = [y ++ " = " ++ show (y0/h) | (y,y0) <- zip species inits]


generateStochPy :: String -> AST.Env -> ConcreteAffinityNetwork Double -> P' Double -> Double -> [Double] -> IO String
generateStochPy filename env network p h inits = case stochPyModel (extractReactionSystem env network p inits) h of
  Right script -> do
    -- putStrLn $ "Python script:\n\n" ++ script
    writeFile filename script
    return script
  Left _ -> undefined

simulateStochPy :: String -> Int -> AST.Env -> ConcreteAffinityNetwork Double -> P' Double -> Double -> [Double] -> IO Trace
simulateStochPy method steps env network p h inits = case stochPyModel (extractReactionSystem env network p inits) h of
  Right stochpy -> do
    -- putStrLn $ "Python script:\n\n" ++ script
    writeFile "/tmp/stochpy.psc" stochpy
    let script = "import os, sys\n" ++
                 "f = open(os.devnull, 'w')\n" ++
                 "stdout = sys.stdout\n" ++
                 "sys.stdout = f\n" ++
                 "import stochpy\n" ++
                 "smod = stochpy.SSA()\n" ++
                 "smod.Model('/tmp/stochpy.psc')\n" ++
                 "smod.DoStochSim(method='" ++ method ++ "', end=" ++
                   show steps ++ ", mode='time')\n" ++
                 "sys.stdout=stdout\n" ++
                 "xss = smod.data_stochsim.getSimData(" ++ L.intercalate "," ["'S" ++ show i ++ "'" | (i,_) <- [0..] `zip` kis] ++ ")\n" ++
                 "xss[:,1:] *= " ++ show h ++ "\n" ++
                 "for xs in xss:\n" ++
                 "    print(' '.join(map('{:.18f}'.format, list(xs))))\n"
    writeFile "stochpy_script.py" script
    res <- runPython script
    let yss = (map (map read.words) $ lines res) :: [[Double]]
    return [ (head ys, toP (tail ys)) | ys <- yss ]
  Left _ -> undefined
  where toP ks = fromList [(k,i) | (k, (_,i)) <- ks `zip` kis]
        kis = toList p
