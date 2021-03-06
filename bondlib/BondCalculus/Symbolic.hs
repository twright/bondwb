module BondCalculus.Symbolic
  (Atom(..), Expr(..), Symbolic(..), SymbolicVars(..), SymbolicExpr, ExprConstant,
   var, valf, vali, simplify, sumToList, prodToList, applyVar,factors) where

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

type ExprConstant a = ( DoubleExpression a
                      , Eq a
                      , Ord a
                      , Nullable a
                      , Num a
                      , Show a
                      , Pretty a
                      -- , Fractional (SymbolicExpr a)
                      -- , Floating (SymbolicExpr a)
                    --   , Symbolic k (SymbolicExpr a)
                      -- , Symbolic a (SymbolicExpr a)
                      -- , Expression (SymbolicExpr a)
                      -- , Nullable (SymbolicExpr a)
                      , Boundable a )

type ExprEnv k = M.Map String k
type SymbolicExpr a = Expr (Atom a)

applyVar :: M.Map String (Atom a) -> SymbolicExpr a -> SymbolicExpr a
applyVar m = fmap f
  where f (Var name) = fromMaybe (Var name) (M.lookup name m)
        f x = x

var :: String -> SymbolicExpr a
var = Atom . Var

instance ExpressionOver a (Atom a) where
    val = Const

instance ExpressionOver a (SymbolicExpr a) where
    val = Atom . Const

valf :: forall a. (DoubleExpression a) => Double -> SymbolicExpr a
valf x = val y
    where y :: a
          y = fromFloat x

vali :: Double -> Double -> Expr (Atom Interval)
vali x y = val $ fromEndpoints (toRational x) (toRational y)

-- vali1 :: Double -> Expr (Atom Interval)
-- vali1 x = vali x x

instance ExprConstant a => DoubleExpression (SymbolicExpr a) where
  fromFloat = valf

instance ExprConstant a => Num (SymbolicExpr a) where
  -- we do a minimal amount of simplification of expressions on 
  -- construction to keep their size from growing excessively
  -- minimal: without recursive simplification

  -- sums
  Atom (Const a) + Atom (Const b) = val (a + b)
  l@(Atom (Const a) `Prod` x) + r@(Atom (Const b) `Prod` y)
    | x == y = val (a + b) * x
    | otherwise = Sum l r
  l@(x `Prod` Atom (Const a)) + r@(Atom (Const b) `Prod` y)
    | x == y = x * val (a + b)
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
    | c == d && simplify (a + b) == simplify c = 1.0
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
  Atom(Const 0.0) * _ = 0.0
  _ * Atom(Const 0.0) = 0.0

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
    | x == y = x**(1 + b)
    | otherwise = Prod x r
  l@(x `Pow` a) * y
    | x == y = x**(a + 1)
    | otherwise = Prod l y

  -- products of fractions
  (Frac a b) * (Frac c d) = (a * c) / (b * d)
  b * (a `Frac` c) = (a * b) / c
  (a `Frac` c) * b = (b * a) / c
  a * b         = Prod a b

  -- negation
  negate (Atom (Const a)) = val (-a)
  negate a      = (-1) * a

  -- absolute values
  abs (Atom (Const a)) = val (abs a)
  -- We assume variables are positive
  abs x@(Atom (Var _)) = x
  abs a  = Abs a

  -- integer coercion
  fromInteger a = valf (fromIntegral a)

  -- sign
  signum        = Sign

instance (Nullable a, DoubleExpression a, Eq a) => Nullable (SymbolicExpr a) where
  isnull = (== valf 0)

instance forall s . ExprConstant s => Fractional (SymbolicExpr s) where
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
  fromRational a = val (fromRational a :: s)

-- expression division helper
frac_ :: ExprConstant a => SymbolicExpr a -> SymbolicExpr a -> SymbolicExpr a
a `frac_` b | a == b = valf 1.0
            | otherwise = Frac a b
  

instance ExprConstant a => Floating (SymbolicExpr a) where
  pi     = valf pi
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

-- Symbolic k a means that given an ExprEnv looking up ks,
-- we can eval an a to get a k
class SymbolicVars a => Symbolic k a where
  eval :: ExprEnv k -> a -> Either [String] k

instance SymbolicVars (Atom a) where
  freeVars (Const _) = []
  freeVars (Var x) = [x]

-- redundant!
-- instance Symbolic a (Atom a) where
--   eval _ (Const x) = Right x
--   eval env (Var v) = case M.lookup v env of
--                        Just x -> Right x
--                        Nothing -> Left ["Variable " ++ v ++ " used but not defined."]

-- a = Atom b
instance (ExpressionOver b k) => Symbolic k (Atom b) where
  eval _ (Const y) = Right $ val y
  eval env (Var v) = case M.lookup v env of
                       Just x -> Right x
                       Nothing -> Left ["Variable " ++ v ++ " used but not defined."]

-- also redundant! (since DoubleExpression k => ExpressionOver Double k)
-- instance DoubleExpression k => Symbolic k (Atom Double) where
--   eval _ (Const x) = Right $ fromFloat x
--   eval env (Var v) = case M.lookup v env of
--                        Just x -> Right x
--                        Nothing -> Left ["Variable " ++ v ++ " used but not defined."]

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

instance SymbolicVars (SymbolicExpr a) where
  freeVars x = L.sort $ L.nub $ foldl1 (++) $ fmap freeVars x

instance (Floating k, Symbolic k (Atom a)) => Symbolic k (SymbolicExpr a) where
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

sumToList :: SymbolicExpr a -> [SymbolicExpr a]
sumToList (a `Sum` b) = sumToList a ++ sumToList b
sumToList a = [a]

prodToList :: SymbolicExpr a -> [SymbolicExpr a]
prodToList (a `Prod` b) = prodToList a ++ prodToList b
prodToList a = [a]

msetIntersect :: (Eq a) => [a] -> [a] -> [a]
msetIntersect xs ys = concat [zipWith (curry fst) (filter (==x) xs) (filter (==x) ys) | x <- L.nub (xs `L.intersect` ys)]

factors :: ExprConstant a => SymbolicExpr a -> (a, [SymbolicExpr a])
factors (a `Prod` b) = (a0*b0, L.sort $ as ++ bs)
  where (a0, as) = factors a
        (b0, bs) = factors b
factors x@(a `Pow` Atom (Const b)) = case singleValue b of
  Just n | n == fromIntegral m -> (a0**fromIntegral m, take (m*length as) (cycle as))
         | otherwise           -> (1.0, [x])
    where m = floor n
          (a0,as) = factors a
  Nothing -> (1.0, [x])
factors x@(a `Sum` b) = if commonCoeff
    then (a0, res)
    else (1.0, if null cf then [x] else res)
  where (a0, as) = factors a
        (b0, bs) = factors b
        commonCoeff = (a0 == b0) && (singleValue a0 /= Just 1.0)
        as' = L.sort (if commonCoeff || (singleValue a0 == Just 1.0) then as else val a0:as)
        bs' = L.sort (if commonCoeff || (singleValue b0 == Just 1.0) then bs else val b0:bs)
        cf = as `msetIntersect` bs
        a' = product (as' L.\\ cf)
        b' = product (bs' L.\\ cf)
        res = L.sort $ (a' + b'):cf
factors (Atom (Const x)) = (x, [])
factors x = (1.0, [x])

-- simplification passes
simps :: (ExprConstant a) => [SymbolicExpr a -> SymbolicExpr a]
simps = [simpAtom, simpProd, simpFrac, simpSum, simpPow, simpElem]
  where 
    simpAtom (Atom x) = Atom x
    simpAtom x = x

    simpFrac (Atom (Const a) `Frac` Atom (Const b)) = val (a/b)
    simpFrac ((Atom (Const (SingleValue 0.0))) `Frac` _) = 1
    simpFrac (Frac a b `Frac` Frac c d) = (a * d) / (b * c)
    simpFrac (Frac a b `Frac` c) = a / (b * c)
    simpFrac (a `Frac` (Atom (Const (SingleValue 1.0)))) = a
    simpFrac (Frac (factors -> (a0, as)) (factors -> (b0, bs))) = simplify a' / simplify b'
      where as' = L.sort (val (a0/b0):as)
            bs' = L.sort bs
            as'' = as' L.\\ bs'
            bs'' = bs' L.\\ as'
            a' = trace ("as'' = " ++ show as'') product as''
            b' = product bs''
    simpFrac x = x

    simpProd (Frac a b `Prod` Frac c d) = (a * c) / (b * d)
    simpProd (b `Prod` (a `Frac` c)) = (b * a) / c
    simpProd ((a `Frac` c) `Prod` b) = (a * b) / c
    simpProd ((Atom(Const (SingleValue 1.0))) `Prod` a) = a
    simpProd ((Atom(Const (SingleValue 0.0))) `Prod` _) = 0
    simpProd (a `Prod` (Atom(Const (SingleValue 1.0)))) = a
    simpProd (a `Prod` (Atom(Const (SingleValue 0.0)))) = 0
    simpProd (Atom(Const a) `Prod` Atom(Const b)) = val (a*b)
    simpProd (y `Prod` Log x) = Log (x**y)
    simpProd (Log x `Prod` y) = Log (x**y)
    simpProd (a `Prod` (b `Sum` c)) = (a*b) + (a*c)
    simpProd ((a `Sum` b) `Prod` c) = (a*c) + (b*c)
    simpProd ((x `Pow` a) `Prod` (y `Pow` b))
      | x == y = x**(a + b)
    simpProd (x `Prod` (y `Pow` b))
      | x == y = x**(1 + b)
    simpProd ((x `Pow` a) `Prod` y)
      | x == y = x**(a + 1)
    simpProd (a `Prod` (b `Pow` Atom(Const (SingleValue (-1.0)))))
        = simplify a / simplify b
    simpProd ((b `Pow` Atom(Const (SingleValue (-1.0)))) `Prod` a)
        = simplify a / simplify b
    simpProd ((simplify -> a) `Prod` (simplify -> b))
      | a >  b = b * a
      | a == b = a ** 2
      | a <  b = a * b
    simpProd x = x

    simpSum (Atom(Const (SingleValue 0.0)) `Sum` a) = a
    simpSum (a `Sum` Atom(Const (SingleValue 0.0))) = a
    simpSum (Atom(Const a) `Sum` Atom(Const b)) = val (a+b)
    simpSum (((e `Prod` a) `Frac` c) `Sum` ((f `Prod` b) `Frac` d))
        | c == d && e == f && simplify (a + b) == simplify c = e
        | c == d && a == b && simplify (e + f) == simplify c = a
        | c == d && e == b && simplify (a + f) == simplify c = e
        | c == d && a == f && simplify (e + b) == simplify c = a
    simpSum ((a `Frac` c) `Sum` (b `Frac` d))
        | Atom (Const x) <- simplify (c / d)
        = (val x * a + b) / d 
    simpSum z@((e `Prod ` a) `Prod` (c `Pow` Atom(Const (SingleValue (-1))))
      `Sum`  (f `Prod` b) `Prod` (d `Pow` Atom(Const (SingleValue (-1)))))
        | c == d && e == f && simplify (a + b) == c = e
        | c == d && a == b && simplify (e + f) == c = a
        | c == d && e == b && simplify (a + f) == c = e
        | c == d && a == f && simplify (e + b) == c = a
    simpSum z@(a `Prod` (c `Pow` Atom(Const (SingleValue (-1))))
      `Sum`   b `Prod` (d `Pow` Atom(Const (SingleValue (-1)))))
        | c == d && simplify (a + b) == c = 1
    simpSum (Sum (simplify -> a) (simplify -> b))
      | a >  b = b + a
      | a == b = 2 * a
      | a <  b = a + b
    simpSum x = x

    simpPow (a `Pow` Atom(Const (SingleValue 0))) = 1
    simpPow (a `Pow` Atom(Const (SingleValue 1))) = a
    simpPow (Atom(Const (SingleValue 0)) `Pow` _) = 0
    simpPow (Atom(Const (SingleValue 1)) `Pow` _) = 1
    simpPow (Atom (Const a) `Pow` Atom(Const b)) = val (a ** b)
    simpPow ((x `Prod` y) `Pow` a) = (x ** a) * (y ** a)
    simpPow ((x `Pow` a) `Pow` b) = x ** (a * b)
    simpPow ((simplify -> a) `Pow` (simplify -> b)) = a**b
    simpPow x = x

    simpElem (Abs (Atom (Const x))) = val (abs x)

    simpElem (Log (Exp x)) = x
    simpElem (Exp (Log x)) = x
    simpElem (Exp (Atom (Const x))) = val (exp x)
    simpElem (Log (Atom (Const x))) = val (log x)

    simpElem (Exp x) = exp $ simplify x
    simpElem (Log x) = log $ simplify x
    simpElem (Sin x) = sin $ simplify x
    simpElem (SinH x) = sinh $ simplify x
    simpElem (ASin x) = asin $ simplify x
    simpElem (ASinH x) = asinh $ simplify x
    simpElem (Cos x) = cos $ simplify x
    simpElem (CosH x) = cosh $ simplify x
    simpElem (ACos x) = acos $ simplify x
    simpElem (ACosH x) = acosh $ simplify x
    simpElem (Tan x) = tan $ simplify x
    simpElem (TanH x) = tanh $ simplify x
    simpElem (ATan x) = atan $ simplify x
    simpElem (ATanH x) = atanh $ simplify x
    simpElem (Abs x) = abs $ simplify x
    simpElem (Sign x) = signum $ simplify x
    simpElem x = x

instance ExprConstant a => Expression (SymbolicExpr a) where
  simplify s | s == s' = trace ("stopped at s=" ++ show s) s
             | otherwise = trace ("simplified s=" ++ show s ++ " -> s'=" ++ show s')
                                 (simplify s')
    where s' = simp s
          simp = foldl (.) id simps
          trace _ = id