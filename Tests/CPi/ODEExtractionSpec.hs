module CPi.ODEExtractionSpec (spec) where

import Prelude hiding ((*>))

import Test.Hspec
-- import Data.Map (Map)
import qualified Data.Map as M
import CPi.ODEExtraction
import CPi.Symbolic
import CPi.Vector
import CPi.Examples (rabbitModel)
import CPi.AST
import CPi.Processes (concretifyAffSpec)
import Debug.Trace
import Data.Either

ode = IVP ( ["R", "F"]
          , [ var "R" - var "F" * var "R"
            , var "F" * var "R" - var "F"]
          , [2, 1])

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
        `shouldBe` Just "(a) .* (abs(b))"
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
        `shouldBe` Just "(a) .* (abs(b))"
  describe "extractIVP" $ do
    it "extracts the correct IVP for rabbitModel" $
      let Defs env _ _ _ = rabbitModel
          Right network = concretifyAffSpec
                          rabbitModel
                          (AffinityNetworkAppl "MassActionRabbits" [])
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
      Right "import sympy as sym\nimport numpy as np\nfrom sympy.abc import t\nfrom scipy.integrate import odeint\nfrom sys import stdout\n\nxs = sym.symbols('R F')\nxts = [x(t) for x in xs]\nodes = [sym.Eq(xts[0].diff(), xts[0] + (-1.0) * ((xts[1]) * (xts[0]))), sym.Eq(xts[1].diff(), (xts[1]) * (xts[0]) + (-1.0) * (xts[1]))]\ny0 = [2.0, 1.0]\nts = np.linspace(0.0,1.0,5)\nrhss = [eqn.rhs for eqn in odes]\nJac = sym.Matrix(rhss).jacobian(xts)\nf = sym.lambdify((xts, t), rhss, modules='numpy')\nJ = sym.lambdify((xts, t), Jac, modules='numpy')\nys = odeint(f, y0, ts, (), J)\nnp.savetxt(stdout, ys)"
    it "translates rabbit model to python code" $
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
