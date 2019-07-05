module BondCalculus.ODEExtraction
  (IVP(..), PrintStyle(..), ODE(..), AsSage(..),
  matlabExpr, sympyExpr, sageExpr, matlabODE, vectorFieldToODEs, sageODE, sageExprAbst,
  sageExprAbst', extractIVP, sympyODE, solveODEPython, printODEPython,
  sympySimplify, runPython, generateSage) where

import BondCalculus.Base
import BondCalculus.Symbolic
import BondCalculus.Processes
import BondCalculus.Vector
import BondCalculus.AST (pretty)
import qualified BondCalculus.AST as AST
import qualified Data.HashMap.Strict as H
import BondCalculus.Simulation (Trace)
import Data.String.Utils
import Text.Printf

-- import qualified Control.Exception as X
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

--import qualified Numeric.LinearAlgebra as LA

import System.IO.Unsafe
-- import qualified System.Process as OS
import System.Process (readProcess)
-- import Data.Maybe

newtype IVP a = IVP ([String], [Expr (Atom a)], [a]) deriving (Show, Eq)
newtype ODE a = ODE ([String], [Expr (Atom a)])

-------------------------------
-- MATLAB script output:
-------------------------------

extractODE :: ExprConstant a => AST.Env -> ConcreteAffinityNetwork -> P' a -> ODE a
extractODE env network p = vectorFieldToODEs p'
  where -- p' must be expressed in the same basis of p
        -- otherwise the variable order becomes
        -- inconsistient, and we get extra variables
        p' = fromList [(p'coeff i, i) | (_,i) <- toList p]
        p'coeff i = fromMaybe 0.0 (H.lookup i v)
        Vect v = dPdt' tr network p
        tr = tracesGivenNetwork network env

extractIVP :: ExprConstant a => AST.Env -> ConcreteAffinityNetwork -> P' a -> [a] -> IVP a
extractIVP env network p = fromODEToIVP (extractODE env network p)

vectorFieldToODEs :: ExprConstant a => P' a -> ODE a
vectorFieldToODEs v = ODE (vars, rhss)
  where
    vars = map (pretty.snd) kis
    rhss = map (simplify.fst) kis
    kis = toList v

fromODEToIVP :: ODE a -> [a] -> IVP a
fromODEToIVP (ODE (x,y)) z = IVP (x, y, z)

-- modelToIVP :: BondCalculusModel (SymbolicExpr Double) -> IVP

matlabODE :: IVP Double -> (Int,(Double,Double)) -> Either String String
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

class Show a => AsSage a where
    asSage :: a -> String

instance AsSage Double where
    asSage = show

instance AsSage Interval where
    asSage x = printf "RIF(%.20f, %.20f)" l u
        where (l, u) = endpoints x
              -- Do some fiddling with the endpoints to ensure sound enclosure
            --   l' = l - 6e-21
            --   u' = u + 6e-21

sageExpr :: AsSage a => M.Map String String -> Expr (Atom a) -> Maybe String
sageExpr mp (Atom (Var x)) = M.lookup x mp
sageExpr mp (Atom (Const x)) = return $ asSage x
sageExpr mp (x `Sum` y) = do
    x' <- sageExpr mp x
    y' <- sageExpr mp y
    return $ x' ++ " + " ++ y'
sageExpr mp (x `Prod` y) = do
    x' <- sageExpr mp x
    y' <- sageExpr mp y
    return $ "(" ++ x' ++ ") * (" ++ y' ++ ")"
sageExpr mp (x `Frac` y) = do
    x' <- sageExpr mp x
    y' <- sageExpr mp y
    return $ "(" ++ x' ++ ") / (" ++ y' ++ ")"
sageExpr mp (x `Pow` y) = do
    x' <- sageExpr mp x
    y' <- sageExpr mp y
    return $ "(" ++ x' ++ ") ** (" ++ y' ++ ")"
sageExpr mp (Abs x) = do
    x' <- sageExpr mp x
    return $ "abs(" ++ x' ++ ")"
sageExpr mp (Sin x) = do
    x' <- sageExpr mp x
    return $ "sin(" ++ x' ++ ")"
sageExpr mp (Cos x) = do
    x' <- sageExpr mp x
    return $ "sin(" ++ x' ++ ")"
sageExpr mp (Tan x) = do
    x' <- sageExpr mp x
    return $ "tan(" ++ x' ++ ")"
sageExpr mp (Exp x) = do
    x' <- sageExpr mp x
    return $ "exp(" ++ x' ++ ")"
sageExpr mp (Log x) = do
    x' <- sageExpr mp x
    return $ "log(" ++ x' ++ ")"

-- Convert a sage expression into a string with all non-simple double constants (e.g. intervals)
-- abstracted by symbolic variables
sageExprAbst' :: (AsSage a, Boundable a) => M.Map String String -> Expr (Atom a) -> Maybe (M.Map String String, String)
sageExprAbst' = sageExprAbst M.empty

sageExprAbst :: (AsSage a, Boundable a) => M.Map String String -> M.Map String String -> Expr (Atom a) -> Maybe (M.Map String String, String)
sageExprAbst cmp mp (Atom (Var x)) = do
    x' <- M.lookup x mp
    return (cmp, x')
sageExprAbst cmp mp (Atom (Const x)) = return $ case singleValue x of
    Just 1.0    -> (cmp, "1")
    Just (-1.0) -> (cmp, "-1")
    Just 0.0    -> (cmp, "0")
    Just v      -> (cmp, asSage v)
    Nothing     -> (cmp', s)
        where n = M.size cmp 
              s = "a" ++ show n
              cmp' = M.insert s (asSage x) cmp 
sageExprAbst cmp mp (x `Sum` y) = f2 cmp mp x y $ \x' y' -> x' ++ " + " ++ y'
sageExprAbst cmp mp (x `Prod` y) = f2 cmp mp x y $ \x' y' -> "(" ++ x' ++ ") * (" ++ y' ++ ")"
sageExprAbst cmp mp (x `Frac` y) = f2 cmp mp x y $ \x' y' -> "(" ++ x' ++ ") / (" ++ y' ++ ")"
sageExprAbst cmp mp (x `Pow` y) = f2 cmp mp x y $ \x' y' -> "(" ++ x' ++ ") ** (" ++ y' ++ ")"
sageExprAbst cmp mp (Abs x) = f1 cmp mp x $ \x' -> "abs(" ++ x' ++ ")"
sageExprAbst cmp mp (Sin x) = f1 cmp mp x $ \x' -> "sin(" ++ x' ++ ")"
sageExprAbst cmp mp (Cos x) = f1 cmp mp x $ \x' -> "cos(" ++ x' ++ ")"
sageExprAbst cmp mp (Tan x) = f1 cmp mp x $ \x' -> "tan(" ++ x' ++ ")"
sageExprAbst cmp mp (Exp x) = f1 cmp mp x $ \x' -> "exp(" ++ x' ++ ")"
sageExprAbst cmp mp (Log x) = f1 cmp mp x $ \x' -> "log(" ++ x' ++ ")"
sageExprAbst cmp mp x = error $ "Support for converting expression " ++ show x ++ " to sage/sympy not implemented!"

f2 cmp mp x y g = do
    (cmp', x') <- sageExprAbst cmp mp x
    (cmp'', y') <- sageExprAbst cmp' mp y
    return (cmp'', g x' y')

f1 cmp mp x g = do
    (cmp', x') <- sageExprAbst cmp mp x
    return (cmp', g x')

-- This assumes we can reduce the ODEs to sage
sageODE :: (AsSage a, Boundable a) => IVP a -> Either String String
sageODE ivp@(IVP (vars, rhss, inits)) = case odeExprs of
        Nothing -> Left "An ODE equation has an unbound variable"
        Just (cmp, odes) -> Right $
            "from sage.all import *\n" ++
            "import sympy as sym\n\n" ++
            "R, x = PolynomialRing(RIF, " ++ show nvars  ++ ", 'x').objgens()\n" ++
            "xsym = sym.var(','.join(map('x{}'.format, range(0," ++ show nvars ++ "))))\n" ++
            (if (M.size cmp) > 0
             then "asym = sym.var(','.join(map('a{}'.format, range(0," ++ show (M.size cmp) ++ "))))\n"
             else "asym = []\n") ++
            "y0 = [" ++ L.intercalate ", " (map asSage inits) ++ "]\n" ++
            "ysymraw = [" ++ L.intercalate ", " odes ++ "]\n" ++
            "ysym = [sym.simplify(y1) for y1 in ysymraw]\n" ++
            "ysage = [y1._sage_().substitute(" ++ subsExpr ++ ") for y1 in ysym]\n" ++
            "try:\n" ++
            "    y = vector([R(y1) for y1 in ysage])\n" ++
            "except TypeError:\n" ++
            "    y = None\n"
            where subsExpr = L.intercalate ","
                             [x ++ "=" ++ y | (x, y) <- M.toList cmp]
    where
    --   odeExprs' :: AsSage a => M.Map String String -> [Expr (Atom a)] -> Maybe (M.Map String String, [String])
      odeExprs' cmp (x:xs) = do
        (cmp', expr) <- sageExprAbst cmp varmap x 
        (cmp'', exprs) <- odeExprs' cmp' xs
        return (cmp'', expr:exprs)
      odeExprs' cmp [] = Just (cmp, [])
      odeExprs :: Maybe (M.Map String String, [String])
      odeExprs = odeExprs' M.empty rhss
      nvars = length vars
      xvars = ["x" ++ show n | (n, _) <- zip [0..] vars]
      varmap = M.fromList $ zip vars xvars
    --   odes = map odeExpr rhss
    --   odeExpr rhs = do
    --     expr <- sageExpr varmap rhs
    --     return $ "sym.simplify(" ++ expr ++ ")"


sympyODE :: IVP Double -> (Int,(Double,Double)) -> Either String String
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

sympyODEPrint :: ODE Double -> PrintStyle -> Either String String
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

matlabExpr :: M.Map String String -> SymbolicExpr Double -> Maybe String
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


sympyExpr :: M.Map String String -> SymbolicExpr Double -> Maybe String
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

callSolveODEPython :: AST.Env -> ConcreteAffinityNetwork -> P' Double -> [Double] -> (Int, (Double, Double)) -> IO String
callSolveODEPython env network p inits tr = case sympyODE (extractIVP env network p inits) tr of
  Right script -> do
    putStrLn $ "Python script:\n\n" ++ script
    writeFile "script.py" script
    runPython script
  Left _ -> undefined

generateSage :: (ExprConstant a, AsSage a)
             => String
             -> AST.Env
             -> ConcreteAffinityNetwork
             -> P' a
             -> [a]
             -> IO String
generateSage filename env network p inits = case sageODE (extractIVP env network p inits) of
  Right script -> do
    putStrLn $ "Sage script:\n\n" ++ script
    writeFile filename script
    return script
  Left _ -> undefined

callPrintODEPython :: AST.Env -> ConcreteAffinityNetwork -> P' Double -> PrintStyle -> IO String
callPrintODEPython env network p style = case sympyODEPrint (extractODE env network p) style of
  Right script -> do
    -- putStrLn $ "Python script:\n\n" ++ script
    writeFile "print_script.py" script
    runPython script
  Left _ -> undefined

solveODEPython :: AST.Env -> ConcreteAffinityNetwork -> P' Double -> [Double] -> (Int, (Double, Double)) -> Trace
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

printODEPython :: AST.Env -> ConcreteAffinityNetwork -> P' Double -> PrintStyle -> String
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
