module BondCalculus.ODEExtraction
  (IVP(..), PrintStyle(..), matlabExpr, sympyExpr, matlabODE, vectorFieldToODEs, extractIVP, sympyODE, solveODEPython, printODEPython, sympySimplify, runPython) where

import BondCalculus.Symbolic
import BondCalculus.Processes
import BondCalculus.Vector
import BondCalculus.AST (pretty)
import qualified BondCalculus.AST as AST
import qualified Data.HashMap.Strict as H
import BondCalculus.Simulation (Trace)
import Data.String.Utils

-- import qualified Control.Exception as X
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

--import qualified Numeric.LinearAlgebra as LA

import System.IO.Unsafe
-- import qualified System.Process as OS
import System.Process (readProcess)
-- import Data.Maybe

newtype IVP = IVP ([String], [SymbolicExpr], [Double]) deriving (Show, Eq)
newtype ODE = ODE ([String], [SymbolicExpr])

-------------------------------
-- MATLAB script output:
-------------------------------

extractODE :: AST.Env -> ConcreteAffinityNetwork -> P' -> ODE
extractODE env network p = vectorFieldToODEs p'
  where -- p' must be expressed in the same basis of p
        -- otherwise the variable order becomes
        -- inconsistient, and we get extra variables
        p' = fromList [(p'coeff i, i) | (_,i) <- toList p]
        p'coeff i = fromMaybe 0.0 (H.lookup i v)
        Vect v = dPdt' tr network p
        tr = tracesGivenNetwork network env

extractIVP :: AST.Env -> ConcreteAffinityNetwork -> P' -> [Double] -> IVP
extractIVP env network p = fromODEToIVP (extractODE env network p)

vectorFieldToODEs :: P' -> ODE
vectorFieldToODEs v = ODE (vars, rhss)
  where
    vars = map (pretty.snd) kis
    rhss = map (simplify.fst) kis
    kis = toList v

fromODEToIVP :: ODE -> [Double] -> IVP
fromODEToIVP (ODE (x,y)) z = IVP (x, y, z)

-- modelToIVP :: BondCalculusModel SymbolicExpr -> IVP

matlabODE :: IVP -> (Int,(Double,Double)) -> Either String String
matlabODE ivp (n,(t0,tn)) =
  if any isNothing eqns
  then Left "An ODE equation has an unbound variable"
  else Right $
  "init = [" ++ L.intercalate ";" (map show inits) ++ "];\n" ++
    "t = linspace(" ++ show t0 ++ "," ++ show tn ++ "," ++ show n ++ ");\n" ++
    "function xdot = f(x,t) \n\n" ++
    L.intercalate ";\n" (catMaybes eqns) ++
    ";\nendfunction;\n\n" ++
    -- Need new support for calculating jacobian
    -- "function jac = jj(x,t)\n\n" ++
    -- matlabJac env p' ++ ";\nendfunction;\n\n" ++
    "x = lsode(f, init, t);\n" ++
    "save (\"-ascii\", \"-\", \"x\");"
      where
        xdots = ["xdot(" ++ show i ++ ")" | i <- [(1::Integer)..]]
        xvars = ["x(" ++ show i ++ ")" | i <- [(1::Integer)..]]
        varmap = M.fromList $ zip vars xvars
        IVP (vars,rhss,inits) = ivp
        eqns = [fmap (\y -> xdot ++ " = " ++ y) (matlabExpr varmap rhs)
               | (xdot,rhs) <- zip xdots rhss]

sympyODE :: IVP -> (Int,(Double,Double)) -> Either String String
sympyODE ivp (n,(t0,tn)) =
  if any isNothing eqns
  then Left "An ODE equation has an unbound variable"
  else Right $
  "import sympy as sym\n" ++
  "import numpy as np\n" ++
  "from sympy.abc import t\n" ++
  "from scipy.integrate import odeint\n" ++
  "import sys\n" ++
  "sys.setrecursionlimit(100000)\n\n" ++
  "xs = [" ++ L.intercalate ","
    ["sym.Function('" ++ show x ++ "')" | x <- vars ] ++ "]\n" ++
  "xts = [x(t) for x in xs]\n" ++
  "odes = [" ++ L.intercalate ", " (catMaybes eqns) ++ "]\n" ++
  "y0 = [" ++ L.intercalate ", " (map show inits) ++ "]\n" ++
  "ts = np.linspace(" ++ show t0 ++ "," ++ show tn ++ "," ++ show n ++ ")\n" ++
  "rhss = [eqn.rhs for eqn in odes]\n" ++
  "Jac = sym.Matrix(rhss).jacobian(xts)\n" ++
  "f = sym.lambdify((xts, t), rhss, modules='numpy')\n" ++
  "J = sym.lambdify((xts, t), Jac, modules='numpy')\n" ++
  -- Try to solve ODEs using symbolic Jacobian
  "try: ys = odeint(f, y0, ts, (), J)\n" ++
  -- Fallback to using method without Jacobian
  "except NameError: ys = odeint(f, y0, ts, ())\n" ++
  -- "print(ys)\n" ++
  "print('\\n'.join([' '.join([('%.18e' % y).replace('nan', '0') for y in ysa]) for ysa in ys]))"
    where
      xvars = ["xts[" ++ show i ++ "]" | i <- [(0::Integer)..]]
      xdots = [xvar ++ ".diff()" | xvar <- xvars]
      varmap = M.fromList $ zip vars xvars
      IVP (vars,rhss,inits) = ivp
      mkeqn xdot y = "sym.Eq(" ++ xdot ++ ", sym.simplify(" ++ y ++ "))"
      eqns = [fmap (mkeqn xdot) (sympyExpr varmap rhs)
             | (xdot,rhs) <- zip xdots rhss]

data PrintStyle = Plain | Pretty | LaTeX | MathML deriving (Eq)

sympyODEPrint :: ODE -> PrintStyle -> Either String String
sympyODEPrint ode style =
  if any isNothing eqns
  then Left "An ODE equation has an unbound variable"
  else Right $
  "import sympy as sym\n" ++
  "import numpy as np\n" ++
  "from sympy.abc import t\n" ++
  "import sys\n" ++
  "sys.setrecursionlimit(100000)\n\n" ++
  -- "sym.init_printing(" ++ printingoptions ++ ")\n\n" ++
  "names = [" ++ L.intercalate "," ["'[" ++ (if style == LaTeX then replace "->" "\\\\rightarrow " (replace " " "\\\\ " x) else x) ++ "]'" | x <- vars] ++ "]\n" ++
  "xs = list(map(sym.Function, names))\n" ++
  "xts = [x(t) for x in xs]\n" ++
  "odes = [" ++ L.intercalate ", " (catMaybes eqns) ++ "]\n" ++
  "rhss = [eqn.rhs for eqn in odes]\n" ++
  case style of
    Pretty -> "for ode in odes: print(sym.pretty(ode))\n"
    Plain -> "for ode in odes: print(ode)\n"
    -- print system of odes as latex (formatted for use in an align environment)
    LaTeX -> "print(sym.latex(odes)[7:-7].replace('=', '& =').replace(', \\\\  ', ' \\\\\\\\\\n').replace('{\\\\left(t \\\\right)}', '').replace('\\\\frac{d}{d t}', '\\\\frac{\\\\mathrm d}{\\\\mathrm d t}').replace('operatorname', 'mathrm'))\n"
    MathML -> "from sympy.printing import print_mathml\nprint_mathml(odes)\n"
    where
      xvars = ["xts[" ++ show i ++ "]" | i <- [(0::Integer)..]]
      xdots = [xvar ++ ".diff()" | xvar <- xvars]
      varmap = M.fromList $ zip vars xvars
      ODE (vars,rhss) = ode
      mkeqn xdot y = "sym.Eq(" ++ xdot ++ ", sym.simplify(" ++ y ++ "))"
      eqns = [fmap (mkeqn xdot) (sympyExpr varmap rhs)
             | (xdot,rhs) <- zip xdots rhss]

matlabExpr :: M.Map String String -> SymbolicExpr -> Maybe String
matlabExpr mp (Atom (Var x)) = M.lookup x mp
matlabExpr _ (Atom (Const x)) = return $ show x
matlabExpr mp (x `Sum` y) = do
  x' <- matlabExpr mp x
  y' <- matlabExpr mp y
  return $ x' ++ " .+ " ++ y'
matlabExpr mp (x `Prod` y) = do
  x' <- matlabExpr mp x
  y' <- matlabExpr mp y
  return $ "(" ++ x' ++ ") .* (" ++ y' ++ ")"
matlabExpr mp (x `Pow` y) = do
  x' <- matlabExpr mp x
  y' <- matlabExpr mp y
  return $ "(" ++ x' ++ ") .** (" ++ y' ++ ")"
matlabExpr mp (Abs x) = do
  x' <- matlabExpr mp x
  return $ "abs(" ++ x' ++ ")"


sympyExpr :: M.Map String String -> SymbolicExpr -> Maybe String
sympyExpr mp (Atom (Var x)) = M.lookup x mp
sympyExpr _ (Atom (Const x)) = return $ show x
sympyExpr mp (x `Sum` y) = do
  x' <- sympyExpr mp x
  y' <- sympyExpr mp y
  return $ x' ++ " + " ++ y'
sympyExpr mp (x `Prod` y) = do
  x' <- sympyExpr mp x
  y' <- sympyExpr mp y
  return $ "(" ++ x' ++ ") * (" ++ y' ++ ")"
sympyExpr mp (x `Frac` y) = do
  x' <- sympyExpr mp x
  y' <- sympyExpr mp y
  return $ "(" ++ x' ++ ") / (" ++ y' ++ ")"
sympyExpr mp (x `Pow` y) = do
  x' <- sympyExpr mp x
  y' <- sympyExpr mp y
  return $ "(" ++ x' ++ ") ** (" ++ y' ++ ")"
sympyExpr mp (Abs x) = do
  x' <- sympyExpr mp x
  -- return $ x'
  return $ "abs(" ++ x' ++ ")"
sympyExpr mp (Sin x) = do
  x' <- sympyExpr mp x
  return $ "sin(" ++ x' ++ ")"
sympyExpr mp (Cos x) = do
  x' <- sympyExpr mp x
  return $ "sin(" ++ x' ++ ")"
sympyExpr mp (Tan x) = do
  x' <- sympyExpr mp x
  return $ "tan(" ++ x' ++ ")"
sympyExpr mp (Exp x) = do
  x' <- sympyExpr mp x
  return $ "exp(" ++ x' ++ ")"
sympyExpr mp (Log x) = do
  x' <- sympyExpr mp x
  return $ "log(" ++ x' ++ ")"

---------------------------------------
-- Using Octave to execute the scripts
---------------------------------------

runPython :: String -> IO String
runPython = readProcess "python3" ["-q"]

callSolveODEPython :: AST.Env -> ConcreteAffinityNetwork -> P' -> [Double] -> (Int, (Double, Double)) -> IO String
callSolveODEPython env network p inits tr = case sympyODE (extractIVP env network p inits) tr of
  Right script -> do
    putStrLn $ "Python script:\n\n" ++ script
    writeFile "script.py" script
    runPython script
  Left _ -> undefined

callPrintODEPython :: AST.Env -> ConcreteAffinityNetwork -> P' -> PrintStyle -> IO String
callPrintODEPython env network p style = case sympyODEPrint (extractODE env network p) style of
  Right script -> do
    -- putStrLn $ "Python script:\n\n" ++ script
    writeFile "print_script.py" script
    runPython script
  Left _ -> undefined

solveODEPython :: AST.Env -> ConcreteAffinityNetwork -> P' -> [Double] -> (Int, (Double, Double)) -> Trace
solveODEPython env network p inits tr@(n,(t0,tn))
  = let raw = unsafePerformIO (callSolveODEPython env network p inits tr)
        ts = [t0 + fromIntegral i*(tn-t0)/fromIntegral n | i <- [0..n]]
        yss = (map (map read.words) $ lines raw) :: [[Double]]
        ys = [fromList (xs `zip` pbasis) | xs <- yss]
        pbasis = map snd $ toList p
    in ts `zip` ys

sympySimplify :: String -> String
sympySimplify s = unsafePerformIO (runPython script)
  where script = "from sympy import simplify, sympify\nimport re\n" ++
                 "print(simplify(sympify(re.sub(r'([0-9]+)\\.0(?![0-9]*[1-9])', r'\\1', " ++ show s ++ "))))"

printODEPython :: AST.Env -> ConcreteAffinityNetwork -> P' -> PrintStyle -> String
printODEPython env network p style
  = let raw = unsafePerformIO (callPrintODEPython env network p style)
    in raw
-- callOctave env p mts p' ts = let
--     script = matlabODE env (wholeProc env p mts) p' ts
--   in do
--     putStrLn $ "Octave Script: \n" ++ script
--     OS.readProcess
--       "octave" ["-q", "--eval", script] []

-- | Solver which calculates the symbolic Jacobian, writes MATLAB code, and executes it with GNU Octave. (General purpose, deals with stiff systems, uses LSODE.)
-- solveODEoctave :: Solver
-- solveODEoctave env p mts p' ts@(n,(t0,tn))
--     = let raw = unsafePerformIO (callOctave env p mts p' ts)
--       in (n>< Map.size p') $ map s2d $ words raw


-- Return the MATLAB script for ODEs
-- matlabScript :: Env
--              -> Process
--              -> MTS
--              -> P'
--              -> (Int, (Double, Double))
--              -> String
-- matlabScript env p mts = matlabODE env (wholeProc env p mts)
