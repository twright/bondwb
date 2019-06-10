import sympy as sym
import numpy as np
from sympy.abc import t
from scipy.integrate import odeint
import sys
sys.setrecursionlimit(100000)

xs = [sym.Function('"S"'),sym.Function('"P"'),sym.Function('"E"')]
xts = [x(t) for x in xs]
odes = [sym.Eq(xts[0].diff(), sym.simplify(((-1.1) * ((xts[2]) * (xts[0]))) / (xts[0] + 2.0))), sym.Eq(xts[1].diff(), sym.simplify(((1.1) * ((xts[2]) * (xts[0]))) / (xts[0] + 2.0))), sym.Eq(xts[2].diff(), sym.simplify(0.0))]
y0 = [100.0, 0.0, 10.0]
ts = np.linspace(0.0,10.0,100)
rhss = [eqn.rhs for eqn in odes]
Jac = sym.Matrix(rhss).jacobian(xts)
f = sym.lambdify((xts, t), rhss, modules='numpy')
J = sym.lambdify((xts, t), Jac, modules='numpy')
try: ys = odeint(f, y0, ts, (), J)
except NameError: ys = odeint(f, y0, ts, ())
print('\n'.join([' '.join([('%.18e' % y).replace('nan', '0') for y in ysa]) for ysa in ys]))