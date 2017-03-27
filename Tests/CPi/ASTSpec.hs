module CPi.ASTSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import CPi.AST
import Data.Ix
import qualified Data.List as L

enzymeE :: Species
enzymeE = Par [Sum [(Unlocated "e", Abs 0 (Sum [(Located "x" 0, AbsBase Nil)]))]]
enzymeS :: Species
enzymeS = Par [Sum [(Unlocated "s", Abs 0 (Sum [(Located "r" 0, AbsBase Nil),
                                          (Located "p" 0, AbsBase Nil)]))]]
enzymeC :: Species
enzymeC = New [0] (Par [Sum [(Located "x" 0, AbsBase Nil)],
                                    Sum [(Located "r" 0, AbsBase Nil),
                                         (Located "p" 0, AbsBase Nil)]])

enzymeEC :: Species
enzymeEC = enzymeE <|> enzymeC

absA :: Abstraction
absA = Abs 0 $ Sum [(Located "x" 0, AbsBase Nil)]

absB :: Abstraction
absB = Abs 1 $ Sum [(Located "y" 0,
                     Abs 0 $ Sum [(Located "z" 0, AbsBase Nil),
                                  (Located "w" 1, AbsBase Nil)])]

absAB :: Abstraction
absAB = Abs 1 $ Par [Sum [(Located "x" 1, AbsBase Nil)],
                     Sum [(Located "y" 0,
                           Abs 0 $ Sum [(Located "z" 0, AbsBase Nil),
                                        (Located "w" 1, AbsBase Nil)])]]

absABB :: Abstraction
absABB = Abs 1 $ Par [Sum [(Located "x" 1, AbsBase Nil)],
                      Sum [(Located "y" 0,
                            Abs 0 $ Sum [(Located "z" 0, AbsBase Nil),
                                         (Located "w" 1, AbsBase Nil)])],
                      Sum [(Located "y" 0,
                            Abs 0 $ Sum [(Located "z" 0, AbsBase Nil),
                                         (Located "w" 1, AbsBase Nil)])]]

spec :: SpecWith ()
spec = do
  describe "pretty" $ do
    let aSum = Sum [(Located "s" 0, AbsBase Nil), (Located "e" 0, AbsBase Nil)]
    it "pretty prints located site e@0" $
      pretty (Located "e" 0) `shouldBe` "e@0"
    it "pretty prints located site s@1" $
      pretty (Located "s" 0) `shouldBe` "s@0"
    it "pretty prints unlocated site s" $
      pretty (Unlocated "s") `shouldBe` "s"
    it "pretty prints sum" $
      pretty aSum `shouldBe` "s@0->0 + e@0->0"
    it "pretty prints E" $
      pretty enzymeE `shouldBe` "e->(0)x@0->0"
    it "pretty prints S" $
      pretty enzymeS `shouldBe` "s->(0)(r@0->0 + p@0->0)"
    it "pretty prints C" $
      pretty enzymeC `shouldBe` "new 0 in x@0->0 | (r@0->0 + p@0->0)"
    it "pretty prints E|C" $
      pretty enzymeEC
        `shouldBe` "e->(0)x@0->0 | (new 0 in x@0->0 | (r@0->0 + p@0->0))"
    it "pretty prints arbitrary species with nonempty strings" $
      property $ \x -> x == Nil || not (null (pretty (x::Species)))
  describe "priority" $ do
    it "gives relatively correct priorities for C and E|C" $
      priority enzymeEC <= priority enzymeC
    it "gives relatively correct priorities for E and E|C" $
      priority enzymeEC > priority enzymeE
  describe "maxLoc" $ do
    it "gives the largest location in E" $
      maxLoc enzymeE `shouldBe` 0
    it "gives the largest location in S" $
      maxLoc enzymeS `shouldBe` 0
    it "gives the largest location in a par with abs" $
      shouldBe
        (maxLoc $ Par [
          Sum [(Unlocated "e",
                Abs 1 $ Sum [(Located "y" 0,
                Abs 0 $ Sum [(Located "x" 0, AbsBase Nil)])])],
          Sum [(Unlocated "s", Abs 1 $ Sum [(Located "z" 1,
                Abs 0 $ Sum [(Located "z" 0, AbsBase Nil)])])]])
        1
  describe "relocate" $ do
    it "relocates 1 in a nested definition" $
      let expr = new [2] (Par
                 [Sum [(Located "x" 0, AbsBase Nil)],
                  Sum [(Located "r" 1, AbsBase Nil),
                       (Located "p" 2, AbsBase Nil)]])
          expr' = new [2] (Par
                  [Sum [(Located "x" 0, AbsBase Nil)],
                   Sum [(Located "r" 3, AbsBase Nil),
                        (Located "p" 2, AbsBase Nil)]])
      in relocate 1 3 expr `shouldBe` expr'
    it "relocates 0 to 1 in the body of absA" $
      let Abs _ s = absA
      in relocate 0 1 s `shouldBe` Sum [(Located "x" 1, AbsBase Nil)]
    it "removes old loc from free locs" $
      property $ \x -> not (null $ L.intersect [0,1] $ boundLocs x) || 0 `notElem` freeLocs (relocate 0 1 (x::Species))
    it  "does not change bound locations with new" $
      relocate 0 1 (new [0] (Sum [(Located "x" 0, AbsBase Nil)]))
        `shouldBe` new [0] (Sum [(Located "x" 0, AbsBase Nil)])
    it  "does not change bound locations with abs" $
      relocate 0 1 (Abs 0 (Sum [(Located "x" 0, AbsBase Nil)]))
        `shouldBe` Abs 0 (Sum [(Located "x" 0, AbsBase Nil)])
  describe "colocate" $ do
    it "can place two simple abstractions at the same location" $
      colocate absA absB `shouldBe` absAB
    it "does not mess up parallel composition" $
      ((absA `colocate` absB) `colocate` absB) `shouldBe` absABB
    it "can colocate with overlapping names" $
      (Abs 0 (Def "" [] [4]) <|> AbsBase (Par [Def "" [] [0,3]]))
        `shouldBe` (Abs 5 (Par [Def "" [] [4], Def "" [] [0,3]]))
    it "releases colocated in normal form" $
      property $ \x y -> let newloc = maximum (0:map (+1) (freeLocs y ++ boundLocs x))
                         in normalForm (concretify $ normalForm $ Abs newloc x <|> AbsBase y)
                            ===
                            normalForm (new [newloc] x <|> y)
    it "handles another case with overlapping names" $
      Abs 11 (Def "" [] [10]) <|> AbsBase (Def "" [] [0,9])
        `shouldBe` Abs 11 (Def "" [] [10] <|> Def "" [] [0,9])
    it "still handles another case with overlapping names after normal forms" $
      normalForm (Abs 11 (Def "" [] [10]) <|> AbsBase (Def "" [] [0,9]))
        `shouldBe` AbsBase (Def "" [] [0,9] <|> Def "" [] [10])
    it "does not care about bound variable name when merging" $
      property $ \x y -> normalForm (Abs 2 (relocate (maxLoc x + 1) (maxLoc x + 2) x) <|> AbsBase y)
                         === normalForm (Abs 1 (relocate (maxLoc x + 2) (maxLoc x + 1) x) <|> AbsBase y)
    it "should colocate concretions to give abstracted parallel composition" $
      property $ \x y -> let l = maxLoc x
                             m = maxLoc y
                             p = max l m
                         in normalForm (Abs l x <|> Abs m y)
                              ===
                              normalForm (Abs p (relocate l p x
                                             <|> relocate m p y))
  describe "boundLocs" $ do
    it "gives the bound locations in a par with abs" $
      shouldBe
        (boundLocs $ Par [
          Sum [(Unlocated "e",
                Abs 1 $ Sum [(Located "y" 0,
                Abs 0 $ Sum [(Located "x" 0, AbsBase Nil)])])],
          Sum [(Unlocated "s", Abs 1 $ Sum [(Located "z" 1,
                Abs 0 $ Sum [(Located "z" 0, AbsBase Nil)])])]])
        [0, 1]
    it "should give a contiguous list of locations on normalized species with no freeLocs" $
      property $ \x -> case boundLocs $ normalForm (x :: Species) of
                         [] -> property True
                         xs@(_:_) -> filter (>=mNotFree) xs === range(mNotFree, maximum xs)
                          where flocs = freeLocs x
                                mNotFree = if null flocs
                                           then 0 else maximum flocs + 1
    it "should give a contiguous list of locations on normalized abstractions" $
      property $ \x -> case boundLocs $ normalForm (x :: Abstraction) of
                         [] -> property True
                         xs@(_:_) -> filter (>=mNotFree) xs === range(mNotFree, maximum xs)
                          where flocs = freeLocs x
                                mNotFree = if null flocs
                                           then 0 else maximum flocs + 1
  describe "simplify" $ do
    it "is idempotent on species" $
      property $ \x -> normalForm (normalForm x) === normalForm (x :: Species)
    it "is idempotent on abstractions" $
      property $ \x -> normalForm (normalForm x) === normalForm (x :: Abstraction)
    it "lowers the maximum index in a complex expression" $
       simplify (Abs 1 (Par [Sum [(Located "x" 1,AbsBase Nil)],Sum [(Unlocated "s",AbsBase (Sum [(Located "p" 1,AbsBase Nil),(Located "r" 1,AbsBase Nil)]))]])) `shouldBe` (Abs 0 (Par [Sum [(Located "x" 0,AbsBase Nil)],Sum [(Unlocated "s",AbsBase (Sum [(Located "p" 0,AbsBase Nil),(Located "r" 0,AbsBase Nil)]))]]))
    it "removes unused binders" $
      shouldBe
        (simplify $ New [1] $ Sum [(Located "x" 0, AbsBase Nil)])
        (Sum [(Located "x" 0, AbsBase Nil)])
    it "keeps used binders" $
      shouldBe
        (simplify $ New [0] $ Sum [(Located "x" 0, AbsBase Nil)])
        (New [0] $ Sum [(Located "x" 0, AbsBase Nil)])
  describe "normalForm" $ do
    it "removes unused binders" $
      shouldBe
        (normalForm $ New [1] $ Sum [(Located "x" 0, AbsBase Nil)])
        (Sum [(Located "x" 0, AbsBase Nil)])
    it "keeps used binders" $
      shouldBe
        (normalForm $ New [0] $ Sum [(Located "x" 0, AbsBase Nil)])
        (New [0] $ Sum [(Located "x" 0, AbsBase Nil)])
    it "recognises commutativity" $
      property $ \x y -> normalForm ((x::Species) <|> (y::Species)) === normalForm ((y::Species) <|> (x::Species))
    it "releases E" $
      property $ \x -> normalForm (new [0] (x <|> Def "E" [] []))
        === normalForm (new [0] (normalForm x) <|> Def "E" [] [])
    it "gives associativity" $
      property $ \x y z -> normalForm ((x <|> y) <|> z :: Species)
                       === normalForm (x <|> (y <|> z) :: Species)
    it "reduces unnecessarily high binder" $
      normalForm (Abs 2 $ Sum [(Located "x" 2, AbsBase Nil)])
        `shouldBe` Abs 0 (Sum [(Located "x" 0, AbsBase Nil)])
    it "removes unused binder" $
      normalForm (Abs 1 $ Sum [(Unlocated "x", AbsBase Nil)])
        `shouldBe` AbsBase (Sum [(Unlocated "x", AbsBase Nil)])
    it "removes empty par" $
      normalForm (Par [Def "E" [] []]) `shouldBe` (Def "E" [] [])
    it "has binder in freeLocs" $
      let prop abst@(Abs l sp) = case normalForm abst of
                                   (Abs l' sp') -> property(l' `elem` freeLocs sp')
                                   (AbsBase _) -> property(l `notElem` freeLocs sp)
          prop abst@(AbsBase sp) = normalForm abst === AbsBase (normalForm sp)
      in property prop
    it "has binder as maxLoc" $
      let prop abst@(Abs l sp) = case normalForm abst of
                                   (Abs l' sp') -> property((maxLoc sp' == 0 && l' == 0) || l' == maxLoc sp' + 1)
                                   (AbsBase _) -> property(l `notElem` freeLocs sp)
          prop abst@(AbsBase sp) = normalForm abst === AbsBase (normalForm sp)
      in property prop
    it "does not change freeLocs" $
      property $ \x -> (L.sort $ L.nub $ freeLocs $ normalForm x) === (L.sort $ L.nub $ freeLocs (x::Species))
    it "gives correct normal form in case with similar locs" $
      normalForm (New [1] (Par [Sum [(Located "z" 0,Abs 0 Nil)],Sum [(Located "z" 1,AbsBase Nil)]]))
        `shouldBe`
        (Par [Sum [(Located "z" 0,AbsBase Nil)],New [0] (Sum [(Located "z" 0,AbsBase Nil)])])
    it "gives correct normal for in case with many overlapping locs" $
        normalForm (New [2] (Par [Sum [(Located "z" 0,Abs 0 Nil)],Sum [(Located "z" 1,AbsBase Nil)],Sum [(Located "y" 2,Abs 0 Nil)]]))
          `shouldBe`
          Par [Sum [(Located "z" 0,AbsBase Nil)],Sum [(Located "z" 1,AbsBase Nil)],New [0] (Sum [(Located "y" 0,AbsBase Nil)])]
