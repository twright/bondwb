module CPi.Processes (Process(..), P, D, norm1, partial, conc,
  direct, multilinear, react) where

import Prelude hiding ((*>), (<*))
import CPi.AST
import CPi.Transitions
 -- Yes, we are using bra-ket notation to represent vectors
 -- in an index free manner
import QuantumVector
import Data.Complex
import qualified Data.List as L

-- Process space
type P = Ket Species
-- Potential interaction space
type D = Ket (Tuple (Tuple Species Abstraction) [Prefix])

data Process = Mixture [(Conc, Species)]

norm1 :: (DiracVector a) => a -> Double
norm1 x = sum $ map magnitude $ components x

normalize1 :: (DiracVector a) => a -> a
normalize1 x | n == 0 = x
             | otherwise = scale (1/n :+ 0.0) x
  where n = norm1 x

conc :: [Prefix] -> D -> Double
conc s = norm1 . (conc' ><)
  where conc' (Ket (_ :* _ :* t)) = d s t |> Ket t

direct :: [Prefix] -> D -> Ket (Tuple Species Abstraction)
direct l = normalize1 . (direct' ><) 
  where direct' (Ket (s :* s' :* m)) = d l m |> Ket (s :* s') 

partial :: Env -> Process -> D
partial env (Mixture ((c,spec):xs)) = (c :+ 0.0) |> foldr (+>) KetZero
                                      [Ket s *> Ket s' *> Ket a
                                      | Trans _ s a s' <- trans spec env]
partial _ (Mixture []) = KetZero

-- multilinear extension of a function
multilinear :: (DiracVector a, DiracVector b) => ([a] -> b) -> [a] -> b
multilinear f = multilinear' f []
  where multilinear' f ys [] = f (reverse ys)
        multilinear' f ys (x:xs) = foldl1 add [scale alpha (multilinear' f (e:ys) xs)
                                   | (alpha, e) <- zip alphas es]
          where es     = basis x
                alphas = components x

react :: [Ket (Tuple Species Abstraction)] -> P
react = multilinear react'
  where react' xs = Ket(concretify $ foldl (<|>) (AbsBase Nil) (map target xs)) +>
                    (-1.0) |> foldl (+>) KetZero (map ((Ket).source) xs)
        source (Ket (spec :* spec')) = spec
        target (Ket (spec :* spec')) = spec'

-- react' :: [(Species, Species)] -> P
-- react' specs = Ket 