{-# LANGUAGE GADTSyntax, TypeSynonymInstances, ConstraintKinds, FlexibleInstances, RankNTypes, IncoherentInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module BondCalculus.Symbolic (Atom(..), Expr(..), Symbolic(..), SymbolicVars(..), SymbolicExpr, IntervalSymbolicExpr, ExprConstant, var, val, valf, vali, simplify, sumToList, prodToList, applyVar,factors) where

import qualified Data.Map as M
import BondCalculus.Base
-- import Data.Functor
-- import Data.Foldable
import qualified Data.List as L
import Data.Bifunctor
import Data.Maybe
import Debug.Trace

data Atom a = Var String
            | Const a
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
type SymbolicExpr = Expr (Atom Double)
type IntervalSymbolicExpr = Expr (Atom Interval)
type ExprConstant a = ( DoubleExpression a
                      , Eq a
                      , Ord a
                      , Nullable a
                      , Num a
                      , Show a
                      -- , Fractional (Expr (Atom a))
                      -- , Floating (Expr (Atom a))
                    --   , Symbolic k (Expr (Atom a))
                      , Symbolic a (Expr (Atom a))
                      -- , Expression (Expr (Atom a))
                      -- , Nullable (Expr (Atom a))
                      , Boundable a )

applyVar :: M.Map String (Atom a) -> Expr (Atom a) -> Expr (Atom a)
applyVar m = fmap f
  where f (Var name) = fromMaybe (Var name) (M.lookup name m)
        f x = x

var :: String -> Expr (Atom a)
var = Atom . Var

val :: a -> Expr (Atom a)
val = Atom . Const

valf :: DoubleExpression a => Double -> Expr (Atom a)
valf = val . fromFloat

vali :: Double -> Double -> Expr (Atom Interval)
vali x y = val $ fromEndpoints (toRational x) (toRational y)

-- vali1 :: Double -> Expr (Atom Interval)
-- vali1 x = vali x x

instance ExprConstant a => DoubleExpression (Expr (Atom a)) where
  fromFloat = valf

instance ExprConstant a => Num (Expr (Atom a)) where
  -- we do a minimal amount of simplification of expressions on 
  -- construction to keep their size from growing excessively
  -- minimal: without recursive simplification

  -- sums
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
  a@(Atom (Const x)) + b = case singleValue x of
    Just 0.0 -> b
    _        -> Sum a b
  a + b@(Atom (Const y)) = case singleValue y of
    Just 0.0 -> a
    _        -> Sum a b
  a + b         = Sum a b

  Atom(Const 1.0) * a = a
  a * Atom(Const 1.0) = a
  Atom(Const 0.0) * _ = val 0.0
  _ * Atom(Const 0.0) = val 0.0

  -- simple products
  Atom(Const a) * Atom(Const b) = val (a*b)
  a@(Atom (Const x)) * b = case singleValue x of
    Just 0.0 -> 0.0
    Just 1.0 -> b
    _        -> Prod a b
  a * b@(Atom (Const y)) = case singleValue y of
    Just 0.0 -> 0.0
    Just 1.0 -> a
    _        -> Prod a b

  -- logs
  y * Log x = Log (x**y)
  Log x * y = Log (x**y)

  -- distributive laws
  -- we do not apply them as they often make things
  -- worse given we don't have any backtracking
  -- a * (b `Sum` c) = (a*b) + (a*c)
  -- (a `Sum` b) * c = (a*c) + (b*c)

  -- products and powers
  l@(x `Pow` a) * r@(y `Pow` b)
    | x == y = x**(a + b)
    | otherwise = Prod l r
  x * r@(y `Pow` b)
    | x == y = x**(val 1 + b)
    | otherwise = Prod x r
  l@(x `Pow` a) * y
    | x == y = x**(a + valf 1)
    | otherwise = Prod l y

  -- products of fractions
  (Frac a b) * (Frac c d) = (a * c) / (b * d)
  b * (a `Frac` c) = (a * b) / c
  (a `Frac` c) * b = (b * a) / c
  a * b         = Prod a b

  -- negation
  negate (Atom (Const a)) = val (-a)
  negate a      = val (-1) * a

  -- absolute values
  abs (Atom (Const a)) = val (abs a)
  -- We assume variables are positive
  abs x@(Atom (Var _)) = x
  abs a  = Abs a

  -- integer coercion
  fromInteger a = valf (fromIntegral a)

  -- sign
  signum        = Sign

instance (Nullable a, DoubleExpression a, Eq a) => Nullable (Expr (Atom a)) where
  isnull = (== valf 0)

instance ExprConstant a => Fractional (Expr (Atom a)) where
  Atom (Const a) / Atom (Const b) = val (a/b)
  -- Atom (Const 0.0) / _ = val 0.0
  Frac a b / Frac c d = (a * d) `frac_` (b * c)
  Frac a b / c = a `frac_` (b * c)
  a / Atom (Const 1.0) = a
  a@(Atom (Const x)) / b = case singleValue x of
    Just 0.0 -> valf 0.0
    _        -> a `frac_` b
  a / b = a `frac_` b

  recip        = (1/)
  fromRational a = val (fromRational a)

-- expression division helper
frac_ :: ExprConstant a => Expr (Atom a) -> Expr (Atom a) -> Expr (Atom a)
a `frac_` b | a == b = valf 1.0
            | otherwise = Frac a b
  

instance ExprConstant a => Floating (Expr (Atom a)) where
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

class SymbolicVars a where
  freeVars :: a -> [String]

class SymbolicVars a => Symbolic k a where
  eval :: ExprEnv k -> a -> Either [String] k

instance SymbolicVars (Atom a) where
  freeVars (Const _) = []
  freeVars (Var x) = [x]

instance Symbolic a (Atom a) where
  eval _ (Const x) = Right x
  eval env (Var v) = case M.lookup v env of
                       Just x -> Right x
                       Nothing -> Left ["Variable " ++ v ++ " used but not defined."]

instance DoubleExpression k => Symbolic k (Atom Double) where
  eval _ (Const x) = Right $ fromFloat x
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

instance SymbolicVars (Expr (Atom a)) where
  freeVars x = L.sort $ L.nub $ foldl1 (++) $ fmap freeVars x

instance (Floating k, Symbolic k (Atom a)) => Symbolic k (Expr (Atom a)) where
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

sumToList :: Expr (Atom a) -> [Expr (Atom a)]
sumToList (a `Sum` b) = sumToList a ++ sumToList b
sumToList a = [a]

prodToList :: Expr (Atom a) -> [Expr (Atom a)]
prodToList (a `Prod` b) = prodToList a ++ prodToList b
prodToList a = [a]

msetIntersect :: (Eq a) => [a] -> [a] -> [a]
msetIntersect xs ys = concat [zipWith (curry fst) (filter (==x) xs) (filter (==x) ys) | x <- L.nub (xs `L.intersect` ys)]

factors :: ExprConstant a => Expr (Atom a) -> (a, [Expr (Atom a)])
factors x@(a `Prod` b) = trace ("factors of x=" ++ show x ++ "\na0 = " ++ show a0 ++ "\nb0 = " ++ show b0)
                               (a0*b0, L.sort $ as ++ bs)
  where (a0, as) = factors a
        (b0, bs) = factors b
factors x@(a `Pow` Atom (Const b)) = case trace ("factors of x = " ++ show x) (singleValue b) of
  Just n | n == fromIntegral m -> (a0**fromIntegral m, take (m*length as) (cycle as))
         | otherwise           -> (1.0, [x])
    where m = floor n
          (a0,as) = factors a
  Nothing -> (1.0, [x])
factors x@(a `Sum` b) = if trace ("factors of x = " ++ show x) commonCoeff
    then trace ("has common") (a0, res)
    else trace ("no common") (1.0, if null cf then trace "evaluating x" [x] else trace "evalutating res" res)
  where (a0, as) = factors a
        (b0, bs) = factors b
        commonCoeff = (a0 == b0) && (singleValue a0 /= Just 1.0)
        as' = L.sort (if commonCoeff || (singleValue a0 == Just 1.0) then as else val a0:as)
        bs' = L.sort (if commonCoeff || (singleValue b0 == Just 1.0) then bs else val b0:bs)
        cf = trace ("evaluating cf") (trace "evaluating as" as `msetIntersect` trace "evaluating bs" bs)
        a' = product (as' L.\\ cf)
        b' = product (bs' L.\\ cf)
        res = L.sort $ (a' + b'):cf
factors (Atom (Const x)) = (x, [])
factors x = (1.0, [x])

instance ExprConstant a => Expression (Expr (Atom a)) where
  -- simplify :: IntervalSymbolicExpr -> IntervalSymbolicExpr
  -- simplify s = s
  simplify s | s == s' = trace ("stopped at s=" ++ show s) s
             | otherwise = trace ("simplified s=" ++ show s ++ " -> s'=" ++ show s')
                                 (simplify s')
    where s' = trace ("about to simplify s=" ++ show s) (simp0 s)

          simp0 :: ExprConstant a => Expr (Atom a) -> Expr (Atom a)
          simp0 (Atom x) = Atom x

          simp0 (Atom (Const a) `Frac` Atom (Const b)) = val (a/b)
          simp0 (a@(Atom (Const x)) `Frac` b) = case singleValue x of
            Just 0.0 -> val 0.0
            _        -> simp1 (a `Frac` b)
          simp0 x = simp1 x

          simp1 (Frac a b `Frac` Frac c d) = (a * d) / (b * c)
          simp1 (Frac a b `Frac` c) = a / (b * c)
          simp1 (a `Frac` b@(Atom (Const x))) = case singleValue x of
            Just 1.0 -> a
            _        -> simp2 (a `Frac` b)
          simp1 x = simp2 x

          -- simp2 (Frac a b) = trace
          --                    (if a /= a' || b /= b'
          --                     then "simp2: wants to replace a{" ++ show a ++ " -> " ++ show a' ++ "} b{" ++ show b ++ " -> " ++ show b' ++ "}"
          --                     else "simp2: noop")
          --                    (simplify a' / simplify b')
          --   where (a0,as) = trace ("factors a = " ++ show a) (factors a)
          --         (b0,bs) = trace ("factors b = " ++ show b) (factors b)
          --         as' = L.sort (val (a0/b0):as)
          --         bs' = L.sort bs
          --         as'' = as' L.\\ bs'
          --         bs'' = bs' L.\\ as'
          --         a' = trace "multiplying as''" (product as'')
          --         b' = trace "multiplying bs''" (product bs'')
          -- fraction simplification currently leading to infinite recursion
          simp2 (Frac a b) = simplify a / simplify b
          simp2 x = trace "simp2: fallthrough" (simp3 x)

          simp3 (a@(Atom(Const x)) `Prod` b) = case singleValue x of
            Just 1.0 -> b
            Just 0.0 -> valf 0
            _        -> simp4 (a `Prod` b)
          simp3 x = trace "simp3: fallthrough" (simp4 x)

          simp4 (a `Prod` b@(Atom(Const y))) = case singleValue y of
            Just 1.0 -> b
            Just 0.0 -> valf 0
            _        -> simp5 (a `Prod` b)
          simp4 x = simp5 x

          simp5 (Atom(Const a) `Prod` Atom(Const b)) = val (a*b)
          simp5 (y `Prod` Log x) = Log (x**y)
          simp5 (Log x `Prod` y) = Log (x**y)
          simp5 (a `Prod` (b `Sum` c)) = (a*b) + (a*c)
          simp5 ((a `Sum` b) `Prod` c) = (a*c) + (b*c)
          simp5 z@((x `Pow` a) `Prod` (y `Pow` b))
            | x == y = x**(a + b)
            | otherwise = simp6 z
          simp5 x = simp6 x
            
          simp6 z@(x `Prod` (y `Pow` b))
            | x == y = x**(val (fromRational 1) + b)
            | otherwise = simp7 z
          simp6 x = simp7 x

          simp7 z@((x `Pow` a) `Prod` y)
            | x == y = x**(a + val (fromRational 1))
            | otherwise = simp8 z
          simp7 x = simp8 x

          simp8 z@((a `Prod` (c `Pow` Atom(Const x))) `Prod` b)
            = case singleValue x of 
              Just (-1.0) -> simplify (b * a) / simplify c
              _           -> simp9 z
          simp8 x = simp9 x

          simp9 z@(b `Prod` (a `Prod` (c `Pow` Atom(Const x))))
            = case singleValue x of 
              Just (-1.0) -> simplify (b * a) / simplify c
              _           -> simp10 z
          simp9 x = simp10 x

          simp10 y@(Atom(Const x) `Sum` _) = case singleValue x of
            Just 0.0 -> valf 0
            _        -> simp11 y
          simp10 x = simp11 x

          simp11 (a `Prod` b)
            | a' > b' = b' * a'
            | a' == b' = a' ** fromFloat 2.0
            | otherwise = a' * b'
            where a' = simplify a
                  b' = simplify b
          simp11 y@(Atom(Const x) `Sum` _) = case singleValue x of
            Just 0.0 -> valf 0
            _        -> simp12 y
          simp11 x = simp12 x

          simp12 (Atom(Const a) `Sum` Atom(Const b)) = val (a+b)
          simp12 z@((e `Prod ` a) `Prod` (c `Pow` Atom(Const x))
            `Sum`  (f `Prod` b) `Prod` (d `Pow` Atom(Const y)))
            = case (singleValue x, singleValue y) of
                (Just (-1.0), Just (-1.0))
                  | c == d && e == f && simplify (a + b) == c -> e
                  | c == d && a == b && simplify (e + f) == c -> a
                  | c == d && e == b && simplify (a + f) == c -> e
                  | c == d && a == f && simplify (e + b) == c -> a
                  | otherwise -> simp13 z
                _ -> simp13 z
          simp12 x = simp13 x
          
          simp13 z@(a `Prod` (c `Pow` Atom(Const x))
            `Sum`   b `Prod` (d `Pow` Atom(Const y)))
            = case (singleValue x, singleValue y) of
                (Just (-1.0), Just (-1.0))
                  | c == d && simplify (a + b) == c -> valf 1.0
                  | otherwise -> simp14 z
                _ -> simp14 z
          simp13 x = simp14 x

          simp14 (Sum a b)
            | a' > b' = b' + a'
            | a' == b' = fromFloat 2.0 * a'
            | otherwise = a' + b'
            where a' = simplify a
                  b' = simplify b
          simp14 y@(a `Pow` Atom(Const x)) = case singleValue x of
            Just 0.0 -> valf 1.0
            Just 1.0 -> a
            _ -> simp15 y
          simp14 x = simp15 x

          simp15 :: ExprConstant a => Expr (Atom a) -> Expr (Atom a)
          simp15 y@(Atom(Const x) `Pow` _) = case singleValue x of
            Just 0.0 -> valf 0.0
            Just 1.0 -> valf 1.0
            _ -> simp16 y
          simp15 x = simp16 x

          simp16 :: ExprConstant a => Expr (Atom a) -> Expr (Atom a)
          simp16 (Atom (Const a) `Pow` Atom(Const b)) = val a ** val b
          simp16 ((x `Prod` y) `Pow` a) = (x ** a) * (y ** a)
          simp16 ((x `Pow` a) `Pow` b) = x ** (a * b)
          simp16 (a `Pow` b) = a'**b'
            where a' = simplify a
                  b' = simplify b

          simp16 (Abs (Atom (Const x))) = val (abs x)

          simp16 (Log (Exp x)) = x
          simp16 (Exp (Log x)) = x
          simp16 (Exp (Atom (Const x))) = val (exp x)
          simp16 (Log (Atom (Const x))) = val (log x)

          simp16 (Exp x) = exp $ simplify x
          simp16 (Log x) = log $ simplify x
          simp16 (Sin x) = sin $ simplify x
          simp16 (SinH x) = sinh $ simplify x
          simp16 (ASin x) = asin $ simplify x
          simp16 (ASinH x) = asinh $ simplify x
          simp16 (Cos x) = cos $ simplify x
          simp16 (CosH x) = cosh $ simplify x
          simp16 (ACos x) = acos $ simplify x
          simp16 (ACosH x) = acosh $ simplify x
          simp16 (Tan x) = tan $ simplify x
          simp16 (TanH x) = tanh $ simplify x
          simp16 (ATan x) = atan $ simplify x
          simp16 (ATanH x) = atanh $ simplify x
          simp16 (Abs x) = abs $ simplify x
          simp16 (Sign x) = signum $ simplify x
          simp16 x = x -- error $ "undefined case: x = "++show x