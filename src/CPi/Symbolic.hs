{-# LANGUAGE GADTSyntax, TypeSynonymInstances, FlexibleInstances, RankNTypes #-}

module CPi.Symbolic (Atom(..), Expr(..), Symbolic(..), SymbolicExpr, var, val, simplify, sumToList, prodToList, applyVar,factors) where

import qualified Data.Map as M
import CPi.Base
-- import Data.Functor
-- import Data.Foldable
import qualified Data.List as L
import Data.Bifunctor
import Data.Maybe

data Atom = Var String
          | Const Double
          deriving (Show, Eq, Ord)

data Expr a where
  Atom  :: a -> Expr a
  Sum   :: Expr a -> Expr a -> Expr a
  Prod  :: Expr a -> Expr a -> Expr a
  Pow   :: Expr a -> Expr a -> Expr a
  Sin   :: Expr a -> Expr a
  SinH  :: Expr a -> Expr a
  ASin  :: Expr a -> Expr a
  ASinH :: Expr a -> Expr a
  Cos   :: Expr a -> Expr a
  CosH  :: Expr a -> Expr a
  ACos  :: Expr a -> Expr a
  ACosH :: Expr a -> Expr a
  Tan   :: Expr a -> Expr a
  TanH  :: Expr a -> Expr a
  Frac  :: Expr a -> Expr a -> Expr a
  ATan  :: Expr a -> Expr a
  ATanH :: Expr a -> Expr a
  Exp   :: Expr a -> Expr a
  Log   :: Expr a -> Expr a
  Abs   :: Expr a -> Expr a
  Sign  :: Expr a -> Expr a
  deriving (Show, Eq, Ord)

type ExprEnv k = M.Map String k
type SymbolicExpr = Expr Atom

applyVar :: M.Map String Atom -> SymbolicExpr -> SymbolicExpr
applyVar m = fmap f
  where f (Var name) = fromMaybe (Var name) (M.lookup name m)
        f x = x

var :: String -> SymbolicExpr
var = Atom . Var

val :: Double -> SymbolicExpr
val = Atom . Const

instance DoubleExpression SymbolicExpr where
  fromFloat = val

instance Num SymbolicExpr where
  Atom (Const 0.0) + a = a
  a + Atom (Const 0.0) = a
  Atom (Const a) + Atom (Const b) = val (a + b)
  l@(Atom (Const a) `Prod` x) + r@(Atom (Const b) `Prod` y)
    | x == y = val (a + b) * x
    | otherwise = Sum l r
  l@(x `Prod` Atom (Const a)) + r@(Atom (Const b) `Prod` y)
    | x == y = val (a + b) * x
    | otherwise = Sum l r
  l@(Atom (Const a) `Prod` x) + r@(y `Prod` Atom (Const b))
    | x == y = val (a + b) * x
    | otherwise = Sum l r
  l@(x `Prod` Atom (Const a)) + r@(y `Prod` Atom (Const b))
    | x == y = x * val (a + b)
    | otherwise = Sum l r
  l@((e `Prod` a) `Frac` c) + r@((f `Prod` b) `Frac` d)
    | c == d && e == f && simplify (a + b) == simplify c = e
    | c == d && a == b && simplify (e + f) == simplify c = a
    | c == d && e == b && simplify (a + f) == simplify c = e
    | c == d && a == f && simplify (e + b) == simplify c = a
    | otherwise = Sum l r
  l@(a `Frac` c) + r@(b `Frac` d)
    | c == d && simplify (a + b) == simplify c = val 1.0
    | otherwise = Sum l r
  a + b         = Sum a b
  Atom(Const 1.0) * a = a
  a * Atom(Const 1.0) = a
  Atom(Const 0.0) * _ = val 0.0
  _ * Atom(Const 0.0) = val 0.0
  Atom(Const a) * Atom(Const b) = val (a*b)
  y * Log x = Log (x**y)
  Log x * y = Log (x**y)
  -- a * (b `Sum` c) = (a*b) + (a*c)
  -- (a `Sum` b) * c = (a*c) + (b*c)
  l@(x `Pow` a) * r@(y `Pow` b)
    | x == y = x**(a + b)
    | otherwise = Prod l r
  x * r@(y `Pow` b)
    | x == y = x**(val 1 + b)
    | otherwise = Prod x r
  l@(x `Pow` a) * y
    | x == y = x**(a + val 1)
    | otherwise = Prod l y
  (Frac a b) * (Frac c d) = (a * c) / (b * d)
  b * (a `Frac` c) = (a * b) / c
  (a `Frac` c) * b = (b * a) / c
  a * b         = Prod a b
  negate (Atom (Const a)) = val (-a)
  negate a      = val (-1) * a
  abs (Atom (Const a)) = val (abs a)
  -- We assume variables are positive
  abs x@(Atom (Var _)) = x
  abs a  = Abs a
  fromInteger a = val (fromIntegral a)
  signum        = Sign

instance Nullable SymbolicExpr where
  isnull = (== val 0)

instance Fractional SymbolicExpr where
  Atom (Const a) / Atom (Const b) = val (a/b)
  Atom (Const 0.0) / _ = val 0.0
  Frac a b / Frac c d = (a * d) / (b * c)
  Frac a b / c = a / (b * c)
  a / Atom (Const 1.0) = a
  a / b
   | a == b    = val 1.0
   | otherwise = Frac a b
  recip        = Frac 1
  fromRational a = val (fromRational a)

instance Floating SymbolicExpr where
  pi     = val pi
  a ** b = a `Pow` b
  exp (Log x) = x
  exp  x  = Exp x
  log (Exp x) = x
  log  x = Log x
  sqrt a = a ** fromRational (1/2)
  sin    = Sin
  asin   = ASin
  sinh   = SinH
  asinh  = ASinH
  cos    = Cos
  acos   = ACos
  cosh   = CosH
  acosh  = ACosH
  tan    = Tan
  tanh   = TanH
  atan   = ATan
  atanh  = ATanH

class Symbolic a where
  freeVars :: a -> [String]
  eval :: forall k . (DoubleExpression k) => ExprEnv k -> a -> Either [String] k

instance Symbolic Atom where
  freeVars (Const _) = []
  freeVars (Var x) = [x]

  eval _ (Const x) = Right (fromFloat x)
  eval env (Var v) = case M.lookup v env of
                       Just x -> Right x
                       Nothing -> Left ["Variable " ++ v ++ " used but not defined."]

instance Foldable Expr where
  foldMap f (Atom x) = f x
  foldMap f (Sum x y) = foldMap f x `mappend` foldMap f y
  foldMap f (Prod x y) = foldMap f x `mappend` foldMap f y
  foldMap f (Frac x y) = foldMap f x `mappend` foldMap f y
  foldMap f (Exp x) = foldMap f x
  foldMap f (Log x) = foldMap f x
  foldMap f (Sin x) = foldMap f x
  foldMap f (ASin x) = foldMap f x
  foldMap f (SinH x) = foldMap f x
  foldMap f (ASinH x) = foldMap f x
  foldMap f (Cos x) = foldMap f x
  foldMap f (ACos x) = foldMap f x
  foldMap f (ACosH x) = foldMap f x
  foldMap f (Tan x) = foldMap f x
  foldMap f (TanH x) = foldMap f x
  foldMap f (ATan x) = foldMap f x
  foldMap f (ATanH x) = foldMap f x
  foldMap f (Pow x y) = foldMap f x `mappend` foldMap f y
  foldMap f (CosH x) = foldMap f x
  foldMap f (Abs x) = foldMap f x
  foldMap f (Sign x) = foldMap f x

instance Functor Expr where
  fmap f (Atom x) = Atom $ f x
  fmap f (Sum x y) = fmap f x `Sum` fmap f y
  fmap f (Prod x y) = fmap f x `Prod` fmap f y
  fmap f (Frac x y) = fmap f x `Frac` fmap f y
  fmap f (Exp x) = Exp $ fmap f x
  fmap f (Log x) = Log $ fmap f x
  fmap f (Sin x) = Sin $ fmap f x
  fmap f (SinH x) = SinH $ fmap f x
  fmap f (ASin x) = ASin $ fmap f x
  fmap f (ASinH x) = ASinH $ fmap f x
  fmap f (Cos x) = Cos $ fmap f x
  fmap f (CosH x) = CosH $ fmap f x
  fmap f (ACos x) = ACos $ fmap f x
  fmap f (ACosH x) = ACosH $ fmap f x
  fmap f (Tan x) = Tan $ fmap f x
  fmap f (TanH x) = TanH $ fmap f x
  fmap f (ATan x) = ATan $ fmap f x
  fmap f (ATanH x) = ATanH $ fmap f x
  fmap f (Pow x y) = fmap f x `Pow` fmap f y
  fmap f (Abs x) = Abs $ fmap f x
  fmap f (Sign x) = Sign $ fmap f x

eitherOp :: (a -> a -> a) -> Either [String] a -> Either [String] a -> Either [String] a
eitherOp op (Right x') (Right y') = Right (x' `op` y')
eitherOp _ (Right _) (Left y')  = Left y'
eitherOp _ (Left x') (Right _)  = Left x'
eitherOp _ (Left x') (Left y')  = Left (x' ++ y')

instance Symbolic SymbolicExpr where
  freeVars x = L.sort $ L.nub $ foldl1 (++) $ fmap freeVars x

  eval env (Atom x) = eval env x
  eval env (Sum x y) = eitherOp (+) (eval env x) (eval env y)
  eval env (Prod x y) = eitherOp (*) (eval env x) (eval env y)
  eval env (Frac x y) = eitherOp (/) (eval env x) (eval env y)
  eval env (Pow x y) = eitherOp (**) (eval env x) (eval env y)
  eval env (Exp x) = second exp $ eval env x
  eval env (Log x) = second log $ eval env x
  eval env (Sin x) = second sin $ eval env x
  eval env (SinH x) = second sinh $ eval env x
  eval env (ASin x) = second asin $ eval env x
  eval env (ASinH x) = second asinh $ eval env x
  eval env (Cos x) = second cos $ eval env x
  eval env (CosH x) = second cosh $ eval env x
  eval env (ACos x) = second acos $ eval env x
  eval env (ACosH x) = second acosh $ eval env x
  eval env (Tan x) = second tan $ eval env x
  eval env (TanH x) = second tanh $ eval env x
  eval env (ATan x) = second atan $ eval env x
  eval env (ATanH x) = second atanh $ eval env x
  eval env (Abs x) = second abs $ eval env x
  eval env (Sign x) = second signum $ eval env x

sumToList :: SymbolicExpr -> [SymbolicExpr]
sumToList (a `Sum` b) = sumToList a ++ sumToList b
sumToList a = [a]

prodToList :: SymbolicExpr -> [SymbolicExpr]
prodToList (a `Prod` b) = prodToList a ++ prodToList b
prodToList a = [a]

factors :: SymbolicExpr -> (Double, [SymbolicExpr])
factors (a `Prod` b) = (a0*b0, as ++ bs)
  where (a0, as) = factors a
        (b0, bs) = factors b
factors x@(a `Pow` Atom (Const n))
  | abs(n - fromIntegral m) < 1e-16 = (a0**fromIntegral m, take (m*length as) (cycle as))
  | otherwise = (1.0, [x])
  where m = floor n
        (a0,as) = factors a
factors (a `Sum` b) = if a0 == b0
    then (a0, res)
    else (1.0, res)
  where (a0, as) = factors a
        (b0, bs) = factors b
        as' = L.sort (if a0 == b0 then as else val a0:as)
        bs' = L.sort (if a0 == b0 then bs else val b0:bs)
        cf = as `L.intersect` bs
        a' = product (as' L.\\ cf)
        b' = product (bs' L.\\ cf)
        res = (a' + b'):cf
factors (Atom (Const x)) = (x, [])
factors x = (1.0, [x])

instance Expression SymbolicExpr where
  simplify s | s == s' = s
             | otherwise = simplify s'
    where s' = simp s

          genSimProd a b
            | a' > b' = b' * a'
            | a' == b' = a' ** 2
            | otherwise = a' * b'
            where a' = simplify a
                  b' = simplify b
          genSimSum a b
            | a' > b' = b' + a'
            | a' == b' = 2.0 * a'
            | otherwise = a' + b'
            where a' = simplify a
                  b' = simplify b

          simp (Atom x) = Atom x

          simp (Atom (Const a) `Frac` Atom (Const b)) = val (a/b)
          simp (Atom (Const 0.0) `Frac` _) = val 0.0
          simp (Frac a b `Frac` Frac c d) = (a * d) / (b * c)
          simp (Frac a b `Frac` c) = a / (b * c)
          simp (a `Frac` Atom (Const 1.0)) = a
          simp (Frac a b) = simplify a' / simplify b'
            where (a0,as) = factors a
                  (b0,bs) = factors b
                  as' = L.sort (val (a0/b0):as)
                  bs' = L.sort bs
                  as'' = as' L.\\ bs'
                  bs'' = bs' L.\\ as'
                  a' = product as''
                  b' = product bs''
          -- simp (Frac a b) = simplify a / simplify b

          simp (Atom(Const 1.0) `Prod` a) = a
          simp (a `Prod` Atom(Const 1.0)) = a
          simp (Atom(Const 0.0) `Prod` _) = val 0.0
          simp (_ `Prod` Atom(Const 0.0)) = val 0.0
          simp (Atom(Const a) `Prod` Atom(Const b)) = val (a*b)
          simp (y `Prod` Log x) = Log (x**y)
          simp (Log x `Prod` y) = Log (x**y)
          simp (a `Prod` (b `Sum` c)) = (a*b) + (a*c)
          simp ((a `Sum` b) `Prod` c) = (a*c) + (b*c)
          simp (l@(x `Pow` a) `Prod` r@(y `Pow` b))
            | x == y = x**(a + b)
            | otherwise = genSimProd l r
          simp (x `Prod` r@(y `Pow` b))
            | x == y = x**(val 1 + b)
            | otherwise = genSimProd x r
          simp (l@(x `Pow` a) `Prod` y)
            | x == y = x**(a + val 1)
            | otherwise = genSimProd l y
          simp ((a `Prod` (c `Pow` Atom(Const (-1)))) `Prod` b)
            = simplify (b * a) / c
          simp (b `Prod` (a `Prod` (c `Pow` Atom(Const (-1)))))
            = simplify (b * a) / c
          simp (a `Prod` b) = genSimProd a b

          simp (Atom(Const 0.0) `Sum` a) = a
          simp (a `Sum` Atom(Const 0.0)) = a
          simp (Atom(Const a) `Sum` Atom(Const b)) = val (a+b)
          simp (l@((e `Prod ` a) `Prod` (c `Pow` Atom(Const (-1))))
            `Sum` r@((f `Prod` b) `Prod` (d `Pow` Atom(Const (-1)))))
            | c == d && e == f && simplify (a + b) == c = e
            | c == d && a == b && simplify (e + f) == c = a
            | c == d && e == b && simplify (a + f) == c = e
            | c == d && a == f && simplify (e + b) == c = a
            | otherwise = genSimSum l r
          simp (l@(a `Prod` (c `Pow` Atom(Const (-1))))
            `Sum` r@(b `Prod` (d `Pow` Atom(Const (-1)))))
            | c == d && simplify (a + b) == c = val 1.0
            | otherwise = genSimSum l r
          simp (Sum a b) = genSimSum a b

          simp (_ `Pow` Atom(Const 0.0)) = val 1.0
          simp (a `Pow` Atom(Const 1.0)) = a
          simp (Atom(Const 0.0) `Pow` _) = val 0.0
          simp (Atom(Const 1.0) `Pow` _) = val 1.0
          simp (Atom (Const a) `Pow` Atom(Const b)) = val a ** val b
          simp ((x `Prod` y) `Pow` a) = (x ** a) * (y ** a)
          simp ((x `Pow` a) `Pow` b) = x ** (a * b)
          simp (a `Pow` b) = a'**b'
            where a' = simplify a
                  b' = simplify b

          simp (Abs (Atom (Const 0))) = val 0

          simp (Log (Exp x)) = x
          simp (Exp (Log x)) = x

          simp (Exp x) = exp $ simplify x
          simp (Log x) = log $ simplify x
          simp (Sin x) = sin $ simplify x
          simp (SinH x) = sinh $ simplify x
          simp (ASin x) = asin $ simplify x
          simp (ASinH x) = asinh $ simplify x
          simp (Cos x) = cos $ simplify x
          simp (CosH x) = cosh $ simplify x
          simp (ACos x) = acos $ simplify x
          simp (ACosH x) = acosh $ simplify x
          simp (Tan x) = tan $ simplify x
          simp (TanH x) = tanh $ simplify x
          simp (ATan x) = atan $ simplify x
          simp (ATanH x) = atanh $ simplify x
          simp (Abs x) = abs $ simplify x
          simp (Sign x) = signum $ simplify x
