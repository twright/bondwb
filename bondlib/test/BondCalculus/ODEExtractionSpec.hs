module BondCalculus.ODEExtractionSpec (spec) where

import Prelude hiding ((*>))

import Test.Hspec
-- import Data.Map (Map)
import qualified Data.Map as M
import BondCalculus.ODEExtraction
import BondCalculus.Symbolic
import BondCalculus.Base
import BondCalculus.Vector
import qualified BondCalculus.Examples as EX
import BondCalculus.AST
import BondCalculus.Processes (concretifyAffSpec, P')
import Debug.Trace
import Data.Either

rabbitModel = EX.rabbitModel :: BondCalculusModel Double

ode = IVP ( ["R", "F"]
          , [ var "R" - var "F" * var "R"
            , var "F" * var "R" - var "F"]
          , [2 :: Double, 1])

spec :: SpecWith ()
spec = do
  describe "matlabExpr" $ do
    it "translates including variables" $
      matlabExpr (M.fromList [("x", "y"), ("y","z")]) ((var "x" + var "y") * var "x")
        `shouldBe`
        Just "(y .+ z) .* (y)"
    it "handles variable not found" $
      matlabExpr (M.fromList [("x", "y"), ("y","z")]) ((var "x" + var "y") * var "w")
        `shouldBe`
        Nothing
    it "translates absolute value" $
      matlabExpr (M.fromList [("x", "a"), ("y", "b")])
                 (var "x" * abs(var "y"))
        `shouldBe` Just "(a) .* (b)"
  describe "sympyExpr" $ do
    it "translates including variables" $
      sympyExpr (M.fromList [("x", "y"), ("y","z")]) ((var "x" + var "y") * var "x")
        `shouldBe`
        Just "(y + z) * (y)"
    it "handles variable not found" $
      sympyExpr (M.fromList [("x", "y"), ("y","z")]) ((var "x" + var "y") * var "w")
        `shouldBe`
        Nothing
    it "translates absolute value" $
      sympyExpr (M.fromList [("x", "a"), ("y", "b")])
                 (var "x" * abs(var "y"))
        `shouldBe` Just "(a) * (b)"
  describe "extractIVP" $ do
    it "extracts the correct IVP for rabbitModel" $
      let Defs env _ _ _ = rabbitModel
          Right network = concretifyAffSpec
                          rabbitModel
                          (AffinityNetworkAppl "MassActionRabbits" [])
          p :: P' Double
          p = var "Rabbit" |> vect (Def "Rabbit" [] []) +>
              var "Fox" |> vect (Def "Fox" [] [])
      in extractIVP env network p [10.0, 1.0]
          `shouldNotBe` IVP ([], [], [])
  -- describe "pythonExpr" $ do
  --   it "translates x to y" $
  --     partial M.empty (Mixture []) `shouldBe` (vectZero :: InteractionVect Conc)
  describe "matlabODE" $ do
    it "translates a simple ODE to matlab code" $
      matlabODE ode (5, (0.0, 1.0))
      `shouldBe`
      Right "init = [2.0;1.0];\nt = linspace(0.0,1.0,5);\nfunction xdot = f(x,t) \n\nxdot(1) = x(1) .+ (-1.0) .* ((x(2)) .* (x(1)));\nxdot(2) = (x(2)) .* (x(1)) .+ (-1.0) .* (x(2));\nendfunction;\n\nx = lsode(f, init, t);\nsave (\"-ascii\", \"-\", \"x\");"
    it "translates rabbit model to matlab code" $
      let Defs env _ _ _ = rabbitModel
          Right network = concretifyAffSpec
                          rabbitModel
                          (AffinityNetworkAppl "MassActionRabbits" [])
          p   = var "Rabbit" |> vect (Def "Rabbit" [] []) +>
                var "Fox" |> vect (Def "Fox" [] [])
          ivp = extractIVP env network p [10.0, 1.0]
      in trace (show ivp) (matlabODE ivp (5, (0.0, 1.0)))
         `shouldSatisfy`
         (\x -> trace (show x) (isRight x))
  describe "sympyODE" $ do
    it "translates a simple ODE to python code" $
      sympyODE ode (5, (0.0, 1.0))
      `shouldBe`
      Right "import sympy as sym\nimport numpy as np\nfrom sympy.abc import t\nfrom scipy.integrate import odeint\nimport sys\nsys.setrecursionlimit(100000)\n\nxs = [sym.Function('\"R\"'),sym.Function('\"F\"')]\nxts = [x(t) for x in xs]\nodes = [sym.Eq(xts[0].diff(), sym.simplify(xts[0] + (-1.0) * ((xts[1]) * (xts[0])))), sym.Eq(xts[1].diff(), sym.simplify((xts[1]) * (xts[0]) + (-1.0) * (xts[1])))]\ny0 = [2.0, 1.0]\nts = np.linspace(0.0,1.0,5)\nrhss = [eqn.rhs for eqn in odes]\nJac = sym.Matrix(rhss).jacobian(xts)\nf = sym.lambdify((xts, t), rhss, modules='numpy')\nJ = sym.lambdify((xts, t), Jac, modules='numpy')\ntry: ys = odeint(f, y0, ts, (), J)\nexcept NameError: ys = odeint(f, y0, ts, ())\nprint('\\n'.join([' '.join([('%.18e' % y).replace('nan', '0') for y in ysa]) for ysa in ys]))"
    it "translates rabbit model to sympy code" $
      let Defs env _ _ _ = rabbitModel
          Right network = concretifyAffSpec
                          rabbitModel
                          (AffinityNetworkAppl "MassActionRabbits" [])
          p   = var "Rabbit" |> vect (Def "Rabbit" [] []) +>
                var "Fox" |> vect (Def "Fox" [] [])
          ivp = extractIVP env network p [10.0, 1.0]
      in trace (show ivp) (sympyODE ivp (5, (0.0, 1.0)))
         `shouldSatisfy`
         (\x -> trace (show x) (isRight x))
  describe "sageExprAbst" $ do
    it "translates including variables" $
      sageExprAbst' (M.fromList [("x", "y"), ("y","z")])
                   ((var "x" + var "y") * var "x" :: SymbolicExpr Double)
        `shouldBe`
        Just (M.empty, "(y + z) * (y)")
    it "handles variable not found" $
      sageExprAbst' (M.fromList [("x", "y"), ("y","z")])
                   ((var "x" + var "y") * var "w" :: SymbolicExpr Double)
        `shouldBe`
        Nothing
    it "translates absolute value" $
      sageExprAbst' (M.fromList [("x", "a"), ("y", "b")])
                    (var "x" * abs(var "y") :: SymbolicExpr Double)
        `shouldBe` Just (M.empty, "(a) * (b)")
    it "translates double" $
        sageExprAbst' (M.fromList [("x", "x")]) (2 * var "x" :: SymbolicExpr Double)
        `shouldBe`
        Just (M.empty, "(2.0) * (x)")
    it "translates singleton interval" $
        sageExprAbst' (M.fromList [("x", "x")])
                      (2 * var "x" :: SymbolicExpr Interval)
        `shouldBe`
        Just (M.empty, "(2.0) * (x)")
    it "translates interval" $
        sageExprAbst' (M.fromList [("x", "x")])
                      (val (fromEndpoints 2.0 3.0) * var "x" :: SymbolicExpr Interval)
        `shouldBe`
        Just ( M.fromList [("a0", "RIF('[2.00000000000000000000 .. 3.00000000000000000000]')")]
             , "(a0) * (x)" )
  describe "sageODE" $ do
    it "translates a simple ODE to python sage code" $
      sageODE ode "{ a || b at rate MA(2) }"
      `shouldBe`
      Right "from sage.all import *\nimport sympy as sym\n\nR, x = PolynomialRing(RIF, 2, ', '.join(map('x{}'.format, range(0, 2)))).objgens()\nxsr = [SR.var(str(x1)) for x1 in x]\nfor v in xsr:\n   assume(v, 'real')\nxsym = sym.var(','.join(map('x{}'.format, range(0,2))))\nasym = []\ny0 = [2.0, 1.0]\nysymraw = [x0 + (-1) * ((x1) * (x0)), (x1) * (x0) + (-1) * (x1)]\nysym = [sym.simplify(y1) for y1 in ysymraw]\nysr = [y1._sage_().substitute() for y1 in ysym]\nvarmap = {'R': x[0], 'F': x[1]}\nvarmapsr = {'R': xsr[0], 'F': xsr[1]}\naffinity_network = \"{ a || b at rate MA(2) }\"\ntry:\n    y = vector([R(y1) for y1 in ysr])\nexcept TypeError:\n    y = None\n"
