{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleContexts #-}

module CPi.NumericalMethods
  (explicitEuler, rungeKutta4, fixedPoint, implicitEuler, implicitTrapezoidal,
   adamsMoulton2, adamsMoulton3, adaptiveAM3, adaptiveModifiedAM3,
   modifiedAM3, gear, gearPred, nordseik, startNordseik)
  where

import CPi.Vector
import Debug.Trace
import Data.Maybe

-- trace :: String -> a -> a
-- trace _ = id

explicitEuler :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Double -> v -> [(Double, v)]
explicitEuler f reduceVect h !t !p0 = (t, p0) : explicitEuler f reduceVect h t' p'
  where t' = t + h
        p' = reduceVect $ p0 +> h |> f p0

rungeKutta4 :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Double -> v -> [(Double, v)]
rungeKutta4 f reduceVect h !t !p0 = (t, p0) : rungeKutta4 f reduceVect h t' p'
  where t'   = t + h
        f'   = f.reduceVect
        !k1  = f' p0
        !k2  = f' $ p0 +> h/2 |> k1
        !k3  = f' $ p0 +> h/2 |> k2
        !k4  = f' $ p0 +> h |> k3
        !p'  = reduceVect
             $ p0 +> h/6 |> (k1 +> 2 |> k2 +> 3 |> k3 +> k4)

fixedPoint :: (Vector Double v) => Double -> Int -> (v -> v) -> (v -> v) -> v -> v
fixedPoint tolerance maxSteps reduceVect f !x
  | maxSteps < 0 = error "Iteration did not converge!"
  | norm (x +> (-1) |> x') < tolerance = x'
  | otherwise = fixedPoint tolerance (maxSteps - 1) reduceVect f x'
  where !x' = trace("x = " ++ show (components x)) $ f $ reduceVect x

implicitEuler :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Int -> Double -> Double -> v -> [(Double, v)]
implicitEuler f reduceVect tolerance maxFPSteps h !t !p0
  = (t, p0) : implicitEuler f reduceVect tolerance maxFPSteps h t' p'
  where !t' = t + h
        -- get initial guess from forward Euler
        -- !q0 = reduceVect $ p0 +> h |> f p0
        !p' = reduceVect $ fixedPoint tolerance maxFPSteps reduceVect s q0
        -- use Runge-Kutta for decent initial guess
        f'   = f.reduceVect
        !q0  = reduceVect $ p0 +> h |> f' p0
        s q = p0 +> h |> f q

implicitTrapezoidal :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Int -> Double -> Double -> v -> [(Double, v)]
implicitTrapezoidal f reduceVect tolerance maxFPSteps h !t !p0
  = (t, p0) : implicitTrapezoidal f reduceVect tolerance maxFPSteps h t' p'
  where !t' = t + h
        -- get initial guess from forward Euler
        -- !q0 = reduceVect $ p0 +> h |> f p0
        !p' = reduceVect $ fixedPoint tolerance maxFPSteps reduceVect s q0
        -- use Runge-Kutta for decent initial guess
        f'   = f.reduceVect
        !q0  = reduceVect $ p0 +> h |> f' p0
        s q = p0 +> h/2 |> (f' q +> f' p0)

adamsMoulton2 :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Int -> Double -> Double -> v -> v -> [(Double, v)]
adamsMoulton2 f reduceVect tolerance maxFPSteps h !t !p0 !p1
  = (t, p0) : adamsMoulton2 f reduceVect tolerance maxFPSteps h t' p1 p'
  where !t' = t + h
        -- get initial guess from forward Euler
        -- !q0 = reduceVect $ p0 +> h |> f p0
        !p' = reduceVect $ fixedPoint tolerance maxFPSteps reduceVect s q0
        -- use Runge-Kutta for decent initial guess
        f'   = f.reduceVect
        !q0  = reduceVect $ p0 +> h |> f' p0
        s q = p1 +> h |> (5/12 |> f' q +> 2/3 |> f' p1 +> (-1/12) |> f' p0)

adamsMoulton3 :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Int -> Double -> Double -> v -> v -> v -> [(Double, v)]
adamsMoulton3 f reduceVect tolerance maxFPSteps h !t0 !p0 !p1 !p2
  = (t0, p0) : adamsMoulton3 f reduceVect tolerance maxFPSteps h t1 p1 p2 p3
  where !t1 = t0 + h
        -- get initial guess from forward Euler
        -- !q0 = reduceVect $ p0 +> h |> f p0
        !p3 = reduceVect $ fixedPoint tolerance maxFPSteps reduceVect s q0
        f'   = f.reduceVect
        -- use forward euler for initial guess
        !q0  = reduceVect $ p2 +> h |> f' p2
        s q = p2 +> h |> (3/8 |> f' q +> 19/24 |> f' p2 +> (-5/24) |> f' p1
                          +> 1/24 |> f' p0)

adaptiveAM3 :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Double -> Double -> Double -> v -> v -> v -> [(Double, v)]
adaptiveAM3 f reduceVect tolerance hmin h !t0 !p0 !p1 !p2
  -- | h < hmin = error $ "step size below minimum limit! with:\nq  = " ++ show q ++ "\nw  = " ++ show w ++ "\nw' = " ++ show w'
  -- if the difference between the predictor and corrector is too great
  -- we need to lower h and recompute
  -- it is harder to see how to adaptively increase h
  | q < 1 && h > hmin = trace ("x(" ++ show t0 ++ ") = " ++ show (components p0)) $ (t0, p0)
          : adaptiveAM3 f reduceVect tolerance hmin hdown t1 p0' p1' p2'
  | q > 4 = (takeWhile ((<t0+4.05*h).fst) $! res)
            ++ (dropWhile ((<t0+4.05*h).fst)
            $! adaptiveAM3 f reduceVect tolerance hmin hup t0 p0'' p1'' p2'')
  | otherwise = res
  where !t1 = t0 + h
        -- !t3' = t0 + h'
        q | diff > 1e-8 = (tolerance * h / diff)**(1/3)
          | otherwise = 5
        diff = norm (w  +> (-1) |> w'')
        -- q'   = (tolerance * 2*h / norm (w  +> (-1) |> w'))**(1/3)
        -- we may have to halve the stepsize before continuing if the error is
        -- too great
        hdown = trace("halving h to " ++ show (h/2)) $ h/2
        hup = trace("doubling h to " ++ show (h*2)) $ 2*h -- be more cautous about increasing step size
        p0' = p1
        p1' = 1/2 |> (p1 +> p2)
        p2' = p2

        (_,!p0''):_:(_,!p1''):_:(_,!p2''):_ = res
        res = trace ("x(" ++ show t0 ++ ") = " ++ show (components p0)) $ (t0, p0) : adaptiveAM3 f reduceVect tolerance hmin h t1 p1 p2 w'

        -- get initial guess from forward Euler
        -- !q0 = reduceVect $ p0 +> h |> f p0
        f'   = f.reduceVect
        f2 = f' p2
        f1 = f' p1
        f0 = f' p0
        -- get predictor guess from Adams-Bashforth
        !w  = reduceVect(p2 +> h |> (23/12 |> f2 +> (-4/3) |> f1 +> 5/12 |> f0))
        -- use Adams-Moulton as a corrector
        !w' = reduceVect (p2 +> h |> (3/8 |> f' w +> 19/24 |> f2
                +> (-5/24) |> f1 +> 1/24 |> f0))
        !w'' = reduceVect (p2 +> h |> (3/8 |> f' w' +> 19/24 |> f2
                +> (-5/24) |> f1 +> 1/24 |> f0))
        -- !p3 = reduceVect $ fixedPoint tolerance maxFPSteps reduceVect s q0
        -- !q0  = reduceVect $ p2 +> h |> f' p2
        -- s q = p2 +> h |> (3/8 |> f' q +> 19/24 |> f' p2 +> (-5/24) |> f' p1
        --                   +> 1/24 |> f' p0)

-- Based on SOLVING IMPLICIT EQUATIONS ARISING FROM ADAMS–MOULTON METHODS
-- TIAN MIN HAN and YUHUAN HAN
adaptiveModifiedAM3 :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Int -> Double -> Double -> Double -> v -> v -> v -> [(Double, v)]
adaptiveModifiedAM3 f reduceVect tolerance maxFPSteps hmin h !t0 !p0 !p1 !p2
  | h < hmin = error "passed minimum stepsize"
  | isJust fp = (t0, p0) : adaptiveModifiedAM3 f reduceVect tolerance maxFPSteps hmin h t1 p1 p2 p3
  | otherwise = (t0, p0) : adaptiveModifiedAM3 f reduceVect tolerance maxFPSteps hmin hdown t1 p0' p1' p2'
  where !t1 = t0 + h
        e = h/2
        hdown = trace("halving h to " ++ show (h/2)) $ h/2
        p0' = p1
        p1' = 1/2 |> (p1 +> p2)
        p2' = p2
        w = h/(h + 8*e/3)
        -- get initial guess from forward Euler
        -- !q0 = reduceVect $ p0 +> h |> f p0
        f2 = f' p2
        f1 = f' p1
        f0 = f' p0
        !q0  = reduceVect(p2 +> h |> (23/12 |> f2 +> (-4/3) |> f1 +> 5/12 |> f0))
        !fp = fixedPoint2 tolerance maxFPSteps reduceVect s q0
        p3 = case fp of
          Just p -> reduceVect p
          Nothing -> undefined
        f'   = f.reduceVect
        -- use forward euler for initial guess
        -- !q0  = reduceVect $ p2 +> h |> f' p2
        adj = (1 - w) |> (p2 +> h |> (19/24 |> f2 +> (-5/24)|> f1
                             +> 1/24 |> f0))
        s q = w |> (e |> f' q +> q) +> adj

fixedPoint2 :: (Vector Double v) => Double -> Int -> (v -> v) -> (v -> v) -> v -> Maybe v
fixedPoint2 tolerance maxSteps reduceVect f !x
  | maxSteps < 0 = Nothing
  | norm (x +> (-1) |> x') < tolerance = Just x'
  | otherwise = fixedPoint2 tolerance (maxSteps - 1) reduceVect f x'
  where !x' = trace("x = " ++ show (components x)) $ f $ reduceVect x

modifiedAM3 :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Int -> Double -> Double -> v -> v -> v -> [(Double, v)]
modifiedAM3 f reduceVect tolerance maxFPSteps h !t0 !p0 !p1 !p2
  = (t0, p0) : modifiedAM3 f reduceVect tolerance maxFPSteps h t1 p1 p2 p3
  where !t1 = t0 + h
        e = h/2
        w = h/(h + 8*e/3)
        -- get initial guess from forward Euler
        -- !q0 = reduceVect $ p0 +> h |> f p0
        !p3 = reduceVect $ fixedPoint tolerance maxFPSteps reduceVect s q0
        f'   = f.reduceVect
        -- get initial guess from Adams-Bashforth
        !q0  = reduceVect(p2 +> h |> (23/12 |> f2 +> (-4/3) |> f1 +> 5/12 |> f0))
        f2 = f' p2
        f1 = f' p1
        f0 = f' p0
        adj = (1 - w) |> (p2 +> h |> (19/24 |> f' p2 +> (-5/24)|> f' p1
                             +> 1/24 |> f' p0))
        s q = w |> (e |> f' q +> q) +> adj


-- nordseik :: (Vector Double v) => (v -> v) -> (v -> v) -> Doub
nordseik :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Double -> Int -> Double -> Double -> Double -> Double -> v -> v -> v -> v -> v -> v -> [(Double, v)]
nordseik g reduceVect tolabs tolrel maxFPSteps hmin hmax h !x !yx !fx !ax !bx !cx !dx
  = case res of
      Just (yxh, fxh, axh, bxh, cxh, dxh) ->
          trace ("-----\nx = " ++ show x ++ "\ny = "
                        ++ show (components yx) ++ "\nfx = "
                        ++ show (components fx) ++ "\n-----")
                ((x, yx) : nordseik g reduceVect tolabs tolrel maxFPSteps hmin hmax h xh yxh fxh axh bxh cxh dxh)
          -- TODO: Implement error control!
      Nothing -> error "did not converge!"
    --  (t', fxh') : rest
  where
    !xh = x + h
    f  = g
    !u   = (yx,fx,ax,bx,cx,dx)
    !res = fmap (\(v1,v2,v3,v4,v5,v6) -> (reduceVect v1, reduceVect v2, reduceVect v3, reduceVect v4, reduceVect v5, reduceVect v6)) $ correctNordseikFP f tolabs tolrel maxFPSteps h u (yx +> h|>(fx +> ax +> bx +> cx +> dx), fx, ax, bx, cx, dx)

nordseik' :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Double -> Int -> Double -> Double -> Double -> Double -> v -> v -> v -> v -> v -> v -> [(Double, v, v, v, v, v, v)]
nordseik' g reduceVect tolabs tolrel maxFPSteps hmin hmax h !x !yx !fx !ax !bx !cx !dx
  = case res of
      Just (yxh, fxh, axh, bxh, cxh, dxh) ->
          trace ("-----\nx = " ++ show x ++ "\ny = "
                        ++ show (components yx) ++ "\nfx = "
                        ++ show (components fx) ++ "\n-----")
                ((x, yx, fx, ax, bx, cx, dx) : nordseik' g reduceVect tolabs tolrel maxFPSteps hmin hmax h xh yxh fxh axh bxh cxh dxh)
          -- TODO: Implement error control!
      Nothing -> error "did not converge!"
    --  (t', fxh') : rest
  where
    !xh = x + h
    f  = g
    !u   = (yx,fx,ax,bx,cx,dx)
    !res = fmap (\(v1,v2,v3,v4,v5,v6) -> (reduceVect v1, reduceVect v2, reduceVect v3, reduceVect v4, reduceVect v5, reduceVect v6)) $ correctNordseikFP f tolabs tolrel maxFPSteps h u (yx +> h|>(fx +> ax +> bx +> cx +> dx), fx, ax, bx, cx, dx)

startNordseik :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Double -> Int -> Int -> Double -> Double -> Double -> Double -> v -> v -> v -> v -> v -> v -> [(Double, v)]
startNordseik g reduceVect tolabs tolrel maxFPSteps maxStartSteps hmin hmax h !x0 !y0 !f0 !a0 !b0 !c0 !d0
  | maxStartSteps < 1 = error "Failed to stabalize at start!"
  | err < tolabs = trace ("=~-~-~= found start! =~-~-~=\n"
                       ++ "\ny = " ++ show (components y0')
                       ++ "\nf = " ++ show (components f0')
                       ++ "\na = " ++ show (components a0')
                       ++ "\nb = " ++ show (components b0')
                       ++ "\nc = " ++ show (components c0')
                       ++ "\nd = " ++ show (components d0') ++ "\n") $
                         map (\(x,y,_,_,_,_,_) -> (x,y))
                         (sim h x0 y0 f0' a0' b0' c0' d0')
  | otherwise = startNordseik g reduceVect tolabs tolrel
                maxFPSteps (maxStartSteps-1) hmin hmax h
                x0 y0 f0 a0' b0' c0' d0'
  where
    sim = nordseik' g reduceVect tolabs tolrel maxFPSteps hmin hmax
    _:_:_:_:(x1,y1,f1,a1,b1,c1,d1):_
      = trace ("===== trying start: ====="
            ++ "\ny = " ++ show (components y0)
            ++ "\nf = " ++ show (components f0)
            ++ "\na = " ++ show (components a0)
            ++ "\nb = " ++ show (components b0)
            ++ "\nc = " ++ show (components c0)
            ++ "\nd = " ++ show (components d0) ++ "\n")
          (sim h x0 y0 f0 a0 b0 c0 d0)
    (x1r,y1r,f1r,a1r,b1r,c1r,d1r) = (x1,y1,f1,(-1)|>a1,b1,(-1)|>c1,d1)
    _:_:_:_:(_,y0',f0',a0',b0',c0',d0'):_ = sim (-h) x1r y1r f1r a1r b1r c1r d1r
    dist a b = norm (a +> (-1)|>b)
    err      = dist y0 y0' `max` dist f0 f0'
                 `max` dist a0 a0' `max` dist b0 b0'
                 `max` dist c0 c0' `max` dist d0 d0'

correctNordseikFP :: (Vector Double v) => (v -> v) -> Double -> Double -> Int -> Double -> (v,v,v,v,v,v) -> (v,v,v,v,v,v) -> Maybe (v,v,v,v,v,v)
correctNordseikFP f tolabs tolrel maxSteps h u@(yx,fx,ax,bx,cx,dx) v@(yxh, fxh, axh, bxh, cxh, dxh)
  | maxSteps < 1 = Nothing
  | err < tolabs -- + tolrel*norm yxh
  -- |   && norm (fxh +> (-1)|>yxh') < tolabs + tolrel*norm yxh
    -- && norm (a +> (-1)|>a') < tolabs + tolrel*norm a
    -- && norm (b +> (-1)|>b') < tolabs + tolrel*norm b
    -- && norm (c +> (-1)|>c') < tolabs + tolrel*norm c
    -- && norm (d +> (-1)|>d') < tolabs + tolrel*norm d
    = trace ("err = " ++ show err ++ "\nyxh (" ++ show maxSteps ++ ") = " ++ show (components yxh) ++ "\nfxh = " ++ show (components fxh) ++ "\nfp = " ++ show (components fp)) $ Just v'
  | otherwise = trace ("err = " ++ show err) $ correctNordseikFP f tolabs tolrel (maxSteps-1) h u $! v'
  where
    dist a b = norm (a +> (-1)|>b)
    err    = dist yxh yxh' `max` dist fxh fxh'
               `max` dist axh axh' `max` dist bxh bxh'
               `max` dist cxh cxh' `max` dist dxh dxh'
    !v'@(yxh',fxh',axh',bxh',cxh',dxh') = correctNordseik f h (fp,ybase,abase,bbase,cbase,dbase) v
    !ybase = yx +> h|>(fx +> ax +>    bx +>    cx +>     dx)
    !fp    =        fx +> 2|>ax +> 3|>bx +> 4|>cx +>  5|>dx
    !abase =                 ax +> 3|>bx +> 6|>cx +> 10|>dx
    !bbase =                          bx +> 4|>cx +> 10|>dx
    !cbase =                                   cx +>  5|>dx
    !dbase =                                             dx

correctNordseik :: (Vector Double v) => (v -> v) -> Double -> (v,v,v,v,v,v) -> (v,v,v,v,v,v) -> (v,v,v,v,v,v)
correctNordseik f h (fp, ybase, abase, bbase, cbase, dbase) (yxh, fxh, axh, bxh, cxh, dxh) = (yxh', fxh', a', b', c', d')
  --  trace ("~~~\nyxh = " ++ show (components yxh) ++ "\nfxh = " ++ show (components fxh) ++ "\naxh = " ++ show (components axh) ++ "\nbxh = " ++ show (components bxh) ++ "\ncxh = " ++ show (components cxh) ++ "\ndxh = " ++ show (components dxh) ++ "\ne'  = " ++ show (components e') ++ "\nfp  = " ++ show (components fp) ++ "\nh    = " ++ show h ++ "\nybase = " ++ show (components ybase) ++ "\n~~~")
      -- (yxh', fxh', a', b', c', d')
  where
    !fxh' = f yxh
    !e'   = fxh'  +> (-1)|>fp
    !yxh' = ybase +> (95*h/288)|>e'
    !a'   = abase +> 25/24|>e'
    !b'   = bbase +> 85/72|>e'
    !c'   = cbase +> 5/48|>e'
    !d'   = dbase +> 1/120|>e'
    -- !y'   = y +> h|>(fx +> a +> b +> c +> d +> 95/288|>e)
    -- !a'   = a +> 3|>b +> 5|>c +> 10|>d +> 25/24|>e
    -- !b'   = b +> 4|>c +> 10|>d +> 35/72|>e
    -- !c'   = c +> 5|>d +> 5/48|>e
    -- !d'   = d +> 1/120|>e

gear :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Double -> Bool -> Int -> Double -> Double -> Double -> v -> v -> v -> v -> v -> [(Double, v)]
gear f reduceVect tolabs tolrel adapt maxFPSteps hmin h !t0 !p0 !p1 !p2 !p3 !p4
  | h < hmin = error "passed minimum stepsize"
  | isNothing fp || q < 1
    = trace ("x(" ++ show t0 ++ ") (halving) = " ++ show (components p0))
      $ (t0, p0) : (t1, p1)
      : gear f reduceVect tolabs tolrel adapt maxFPSteps hmin h' (t0 + 2*h) p0' p1' p2' p3' p4'
  | adapt && q > 4 && (t1'' `approxeq` (t0 + h''))
                   && (t2'' `approxeq` (t0 + 2*h''))
                   && (t3'' `approxeq` (t0 + 3*h''))
                   && (t4'' `approxeq` (t0 + 4*h''))
    = trace "doubling!" pre ++ (dropWhile ((<t0+4.05*h'').fst)
            $! gear f reduceVect tolabs tolrel adapt maxFPSteps hmin h'' t0 p0'' p1'' p2'' p3'' p4'')
  | otherwise = res
  -- trace("t1'' = " ++ show t1'' ++ ", t0 + h = " ++ show (t0 + h''))
  where !t1 = t0 + h
        res | isJust fp = trace ("x(" ++ show t0 ++ ") = \n" ++ show (components p0) ++ "\nsticking with q = " ++ show q) ((t0, p0)
            : gear f reduceVect tolabs tolrel adapt maxFPSteps hmin h t1 p1 p2 p3 p4 p5)
            | otherwise = undefined
        res'' = (t0, p0)
            : gear f reduceVect tolabs tolrel False maxFPSteps hmin h t1 p1 p2 p3 p4 p5
            -- ++ "\nt1'' = " ++ show t1'' ++ ", t0 + h = " ++ show (t0 + h'')
            -- ++ if isNothing fp then "" else ("\nq = " ++ show q ++ "\ndiff = " ++ show diff
        -- get initial guess from Adams-Bashforth
        -- !q0  = reduceVect (p4 +> h |> (1901/720 |> f4 +> (-1387/360) |> f3
        --          +> 109/30 |> f2 +> (-637/360) |> f1 +> 251/720 |> f0))
        -- get initial guess from Adams-Bashforth
        !q0  = reduceVect (p4 +> h |> (1901/720 |> f4 +> (-1387/360) |> f3
                 +> 109/30 |> f2 +> (-637/360) |> f1 +> 251/720 |> f0))
        -- !q0 = reduceVect ((-65/12) |> f4 +> 120/12 |> f3 +> (-60/12) |> f2
        --         +> 20/12 |> f1 +> (-3/12) |> f0 +> 60/12 |> p4)
        -- !q0 = reduceVect ((-65/12) |> p4 +> 120/12 |> p3 +> (-60/12) |> p2
                -- +> 20/12 |> p1 +> (-3/12) |> p0 +> 60/12 |> f4)

        -- iteratively improve using Gear (implicit backwards difference
        -- formula)
        !adj = reduceVect (300/137 |> p4 +> (-300/137) |> p3
                             +> 200/137 |> p2 +> (-75/137) |> p1
                             +> 12/137 |> p0)
        -- !adj = reduceVect ()
        iter !p = (60/137 * h) |> f' p +> adj
        fp = fixedPoint3 tolabs tolrel maxFPSteps reduceVect iter q0
        p5 = reduceVect $ fromMaybe undefined fp

        -- step size adjustor
        ep = ((20/343)/(95/288 + 20/343)) * norm (p5 +> (-1) |> q0)
        q = (tolabs + tolrel * norm p5)/ep
        -- q | isNothing fp = 0
        --   | diff > 1e-10 = ((tolabs + tolrel * norm p5) * h / diff)**(1/6)
        --   | otherwise = 5
        -- diff = norm (p5  +> (-1) |> q0)

        -- we may need to halve h if the accuracy is too low
        h' = trace("halving to h = " ++ show (h/2) ++ (if isNothing fp then " (iteration did not converge)" else " with q = " ++ show q ++ "\np5: " ++ show (components p5) ++ "\nq0:" ++ show (components q0))) $ h/2
        p0' = p2
        p1' = 1/2 |> (p2 +> p3)
        p2' = p3
        p3' = 1/2 |> (p3 +> p4)
        p4' = p4

        -- or we may need to double it
        h'' = trace("doubling to h = " ++ show (2*h) ++ " with q = " ++ show q) 2*h
        u `approxeq` v = trace("comparing " ++ show u ++ " and " ++ show v) abs(u - v) < 1e-8
        pre = trace "evaluating pre" $ take 9 res''
        [(_,p0''),_,(t1'',p1''),_,(t2'',p2''),_,(t3'',p3''),_,(t4'',p4'')]
          = trace("evaluating front with adapt = " ++ show adapt) pre

        -- f values
        f4 = f' p4
        f3 = f' p3
        f2 = f' p2
        f1 = f' p1
        f0 = f' p0
        f' = f.reduceVect

gearPred :: (Vector Double v) => (v -> v) -> (v -> v) -> Double -> Double -> Int -> Double -> Double -> Double -> v -> v -> v -> v -> v -> [(Double, v)]
gearPred f reduceVect tolabs tolrel maxFPSteps hmin h !t0 !p0 !p1 !p2 !p3 !p4 = res
  where !t1 = t0 + h
        res = (t0, p0)
            : gearPred f reduceVect tolabs tolrel maxFPSteps hmin h t1 p1 p2 p3 p4 q0
        -- get initial guess from Adams-Bashforth
        !q0  = reduceVect (p4 +> h |> (1901/720 |> f4 +> (-1387/360) |> f3
                 +> 109/30 |> f2 +> (-637/360) |> f1 +> 251/720 |> f0))
        -- !q0 = reduceVect ((-65/12) |> p4 +> 120/12 |> p3 +> (-60/12) |> p2
        --         +> 20/12 |> p1 +> (-3/12) |> p0 +> 60/12 |> f4)
        -- f values
        f4 = f' p4
        f3 = f' p3
        f2 = f' p2
        f1 = f' p1
        f0 = f' p0
        f' = f.reduceVect

fixedPoint3 :: (Vector Double v) => Double -> Double -> Int -> (v -> v) -> (v -> v) -> v -> Maybe v
fixedPoint3 tolabs tolrel maxSteps reduceVect f !x
  | maxSteps < 0 = Nothing
  | norm (x +> (-1) |> x') < tolabs = Just x'
  | otherwise = fixedPoint3 tolabs tolrel (maxSteps - 1) reduceVect f x'
  where !x' = f $ reduceVect x
