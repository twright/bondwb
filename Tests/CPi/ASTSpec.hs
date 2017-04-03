module CPi.ASTSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import CPi.AST
import qualified Data.List as L

enzymeE :: Species
enzymeE = mkPar [mkSum [(Unlocated "e", mkAbs 0 (mkSum [(Located "x" 0, mkAbsBase Nil)]))]]
enzymeS :: Species
enzymeS = mkPar [mkSum [(Unlocated "s", mkAbs 0 (mkSum [(Located "r" 0, mkAbsBase Nil),
                                          (Located "p" 0, mkAbsBase Nil)]))]]
enzymeC :: Species
enzymeC = new [0] (mkPar [mkSum [(Located "x" 0, mkAbsBase Nil)],
                                    mkSum [(Located "r" 0, mkAbsBase Nil),
                                         (Located "p" 0, mkAbsBase Nil)]])

enzymeEC :: Species
enzymeEC = enzymeE <|> enzymeC

absA :: Abstraction
absA = mkAbs 0 $ mkSum [(Located "x" 0, mkAbsBase Nil)]

absB :: Abstraction
absB = mkAbs 1 $ mkSum [(Located "y" 0,
                     mkAbs 0 $ mkSum [(Located "z" 0, mkAbsBase Nil),
                                  (Located "w" 1, mkAbsBase Nil)])]

absAB :: Abstraction
absAB = mkAbs 1 $ mkPar [mkSum [(Located "x" 1, mkAbsBase Nil)],
                     mkSum [(Located "y" 0,
                           mkAbs 0 $ mkSum [(Located "z" 0, mkAbsBase Nil),
                                        (Located "w" 1, mkAbsBase Nil)])]]

absABB :: Abstraction
absABB = mkAbs 1 $ mkPar [mkSum [(Located "x" 1, mkAbsBase Nil)],
                      mkSum [(Located "y" 0,
                            mkAbs 0 $ mkSum [(Located "z" 0, mkAbsBase Nil),
                                         (Located "w" 1, mkAbsBase Nil)])],
                      mkSum [(Located "y" 0,
                            mkAbs 0 $ mkSum [(Located "z" 0, mkAbsBase Nil),
                                         (Located "w" 1, mkAbsBase Nil)])]]

spec :: SpecWith ()
spec = do
  describe "pretty" $ do
    let aSum = mkSum [(Located "s" 0, mkAbsBase Nil), (Located "e" 0, mkAbsBase Nil)]
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
        (maxLoc $ mkPar [
          mkSum [(Unlocated "e",
                mkAbs 1 $ mkSum [(Located "y" 0,
                mkAbs 0 $ mkSum [(Located "x" 0, mkAbsBase Nil)])])],
          mkSum [(Unlocated "s", mkAbs 1 $ mkSum [(Located "z" 1,
                mkAbs 0 $ mkSum [(Located "z" 0, mkAbsBase Nil)])])]])
        1
  describe "relocate" $ do
    it "relocates 1 in a nested definition" $
      let expr = new [2] (mkPar
                 [mkSum [(Located "x" 0, mkAbsBase Nil)],
                  mkSum [(Located "r" 1, mkAbsBase Nil),
                       (Located "p" 2, mkAbsBase Nil)]])
          expr' = new [2] (mkPar
                  [mkSum [(Located "x" 0, mkAbsBase Nil)],
                   mkSum [(Located "r" 3, mkAbsBase Nil),
                        (Located "p" 2, mkAbsBase Nil)]])
      in relocate 1 3 expr `shouldBe` expr'
    it "relocates 0 to 1 in the body of absA" $
      let Abs _ _ s = absA
      in relocate 0 1 s `shouldBe` mkSum [(Located "x" 1, mkAbsBase Nil)]
    it "removes old loc from free locs" $
      property $ \x -> not (null $ L.intersect [0,1] $ boundLocs x) || 0 `notElem` freeLocs (relocate 0 1 (x::Species))
    it  "does not change bound locations with new" $
      relocate 0 1 (new [0] (mkSum [(Located "x" 0, mkAbsBase Nil)]))
        `shouldBe` new [0] (mkSum [(Located "x" 0, mkAbsBase Nil)])
    it  "does not change bound locations with abs" $
      relocate 0 1 (mkAbs 0 (mkSum [(Located "x" 0, mkAbsBase Nil)]))
        `shouldBe` mkAbs 0 (mkSum [(Located "x" 0, mkAbsBase Nil)])
  describe "colocate" $ do
    it "can place two simple abstractions at the same location" $
      colocate absA absB `shouldBe` absAB
    it "does not mess up parallel composition" $
      ((absA `colocate` absB) `colocate` absB) `shouldBe` absABB
    it "can colocate with overlapping names" $
      (mkAbs 0 (Def "" [] [4]) <|> mkAbsBase (mkPar [Def "" [] [0,3]]))
        `shouldBe` (mkAbs 5 (mkPar [Def "" [] [4], Def "" [] [0,3]]))
    it "releases colocated in normal form" $
      property $ \x y -> let newloc = maximum (0:map (+1) (freeLocs y ++ boundLocs x))
                         in normalForm (concretify $ normalForm (mkAbs newloc x <|> mkAbsBase y))
                            ===
                            normalForm (new [newloc] x <|> y)
    it "handles another case with overlapping names" $
      mkAbs 11 (Def "" [] [10]) <|> mkAbsBase (Def "" [] [0,9])
        `shouldBe` mkAbs 11 (Def "" [] [10] <|> Def "" [] [0,9])
    it "still handles another case with overlapping names after normal forms" $
      normalForm (mkAbs 11 (Def "" [] [10]) <|> mkAbsBase (Def "" [] [0,9]))
        `shouldBe` mkAbsBase (Def "" [] [0,9] <|> Def "" [] [10])
    it "does not care about bound variable name when merging" $
      property $ \x y -> normalForm (mkAbs (maxLoc x + 2) (relocate (maxLoc x + 1) (maxLoc x + 2) x) <|> mkAbsBase y)
                         === normalForm (mkAbs (maxLoc x + 1) (relocate (maxLoc x + 2) (maxLoc x + 1) x) <|> mkAbsBase y)
    it "should colocate abstractions to give abstracted parallel composition" $
      property $ \x y -> let l = maxLoc x
                             m = maxLoc y
                             p = max l m
                         in normalForm (mkAbs l x <|> mkAbs m y)
                              ===
                              normalForm (mkAbs p (relocate l p x
                                             <|> relocate m p y))
  describe "boundLocs" $ do
    it "gives the bound locations in a par with abs" $
      shouldBe
        (boundLocs $ mkPar [
          mkSum [(Unlocated "e",
                mkAbs 1 $ mkSum [(Located "y" 0,
                mkAbs 0 $ mkSum [(Located "x" 0, mkAbsBase Nil)])])],
          mkSum [(Unlocated "s", mkAbs 1 $ mkSum [(Located "z" 1,
                mkAbs 0 $ mkSum [(Located "z" 0, mkAbsBase Nil)])])]])
        [0, 1]
    -- it "should give a contiguous list of locations on normalized species with no freeLocs" $
    --   property $ \x -> case boundLocs $ normalForm (x :: Species) of
    --                      [] -> property True
    --                      xs@(_:_) -> filter (>=mNotFree) xs === range(mNotFree, maximum xs)
    --                       where flocs = freeLocs x
    --                             mNotFree = if null flocs
    --                                        then 0 else maximum flocs + 1
    -- it "should give a contiguous list of locations on normalized abstractions" $
    --   property $ \x -> case boundLocs $ normalForm (x :: Abstraction) of
    --                      [] -> property True
    --                      xs@(_:_) -> filter (>=mNotFree) xs === range(mNotFree, maximum xs)
    --                       where flocs = freeLocs x
    --                             mNotFree = if null flocs
    --                                        then 0 else maximum flocs + 1
  describe "normalForm" $ do
    it "simplifies abstractions within sums" $
      normalForm(mkSum [(Located "y" 2,mkAbs 1 Nil)])
        `shouldBe` normalForm(mkSum [(Located "y" 2,mkAbsBase Nil)])
    it "is idempotent on species" $
      property $ \x -> normalForm (normalForm x) === normalForm (x :: Species)
    it "is idempotent on abstractions" $
      property $ \x -> normalForm (normalForm x) === normalForm (x :: Abstraction)
    it "lowers the maximum index in a complex expression" $
       simplify (mkAbs 1 (mkPar [mkSum [(Located "x" 1,mkAbsBase Nil)],mkSum [(Unlocated "s",mkAbsBase (mkSum [(Located "p" 1,mkAbsBase Nil),(Located "r" 1,mkAbsBase Nil)]))]])) `shouldBe` (mkAbs 0 (mkPar [mkSum [(Located "x" 0,mkAbsBase Nil)], mkSum [(Unlocated "s",mkAbsBase (mkSum [(Located "p" 0,mkAbsBase Nil),(Located "r" 0,mkAbsBase Nil)]))]]))
    it "removes unused binders" $
      shouldBe
        (normalForm $ new [1] $ mkSum [(Located "x" 0, mkAbsBase Nil)])
        (mkSum [(Located "x" 0, mkAbsBase Nil)])
    it "keeps used binders" $
      shouldBe
        (normalForm $ new [0] $ mkSum [(Located "x" 0, mkAbsBase Nil)])
        (new [0] $ mkSum [(Located "x" 0, mkAbsBase Nil)])
    it "recognises commutativity" $
      property $ \x y -> normalForm ((x::Species) <|> (y::Species)) === normalForm ((y::Species) <|> (x::Species))
    it "relocates restricted names into canonical order" $
      normalForm(mkPar [mkNew [4,5] (mkSum [(Located "x" 1,mkAbsBase (Nil)),(Located "x" 2,mkAbsBase (Nil)),(Located "x" 3,mkAbs 0 (Def "E" [] [3,0,4])),(Located "x" 5,mkAbsBase (Nil))]),Def "E" [] []])
        `shouldBe`
        normalForm(mkPar [mkNew [4,5] (mkSum [(Located "x" 1,mkAbsBase Nil),(Located "x" 2,mkAbsBase (Nil)),(Located "x" 3,mkAbs 0 (Def "E" [] [3,0,5])),(Located "x" 4,mkAbsBase Nil)]),Def "E" [] []])
    it "releases E with name order issues" $
      let s = mkNew [8] (mkSum [(Located "x" 2,mkAbs 3 Nil),(Located "x" 1,mkAbs 4 (mkSum [])),(Located "x" 3,mkAbs 4 (Def "E" [] [3,4,8])),(Located "x" 0,mkAbs 4 (mkSum []))])
          e = Def "E" [] []
      in normalForm (new [0] (s <|> e))
        `shouldBe` normalForm((new [0] s) <|> e)
    it "releases E" $
      property $ \x -> normalForm (new [0] (x <|> Def "E" [] []))
        === normalForm (new [0] (normalForm x) <|> Def "E" [] [])
    it "gives associativity" $
      property $ \x y z -> normalForm ((x <|> y) <|> z :: Species)
                       === normalForm (x <|> (y <|> z) :: Species)
    it "reduces unnecessarily high binder" $
      normalForm (mkAbs 2 $ mkSum [(Located "x" 2, mkAbsBase Nil)])
        `shouldBe` mkAbs 0 (mkSum [(Located "x" 0, mkAbsBase Nil)])
    it "removes unused binder" $
      normalForm (mkAbs 1 $ mkSum [(Unlocated "x", mkAbsBase Nil)])
        `shouldBe` mkAbsBase (mkSum [(Unlocated "x", mkAbsBase Nil)])
    it "removes empty par" $
      normalForm (mkPar [Def "E" [] []]) `shouldBe` (Def "E" [] [])
    it "has binder in freeLocs" $
      let prop abst@(Abs _ l sp) = case normalForm abst of
                                   (Abs _ l' sp') -> property(l' `elem` freeLocs sp')
                                   (AbsBase _ _) -> property(l `notElem` freeLocs sp)
          prop abst@(AbsBase _ sp) = normalForm abst === mkAbsBase (normalForm sp)
      in property prop
    it "does not change freeLocs" $
      property $ \x -> (L.sort $ L.nub $ freeLocs $ normalForm x) === (L.sort $ L.nub $ freeLocs (x::Species))
    it "gives correct normal form in case with similar locs" $
      normalForm (new [1] (mkPar [mkSum [(Located "z" 0,mkAbs 0 Nil)],mkSum [(Located "z" 1,mkAbsBase Nil)]]))
        `shouldBe`
        mkPar [mkSum [(Located "z" 0,mkAbsBase Nil)],new [0] (mkSum [(Located "z" 0,mkAbsBase Nil)])]
    it "gives correct normal for in case with many overlapping locs" $
      normalForm (new [2] (mkPar [mkSum [(Located "z" 0,mkAbs 0 Nil)],mkSum [(Located "z" 1,mkAbsBase Nil)],mkSum [(Located "y" 2,mkAbs 0 Nil)]]))
        `shouldBe`
        mkPar [new [0] (mkSum [(Located "y" 0,mkAbsBase Nil)]),mkSum [(Located "z" 1,mkAbsBase Nil)],mkSum [(Located "z" 0,mkAbsBase Nil)]]
    it "correctly simplifies nested news" $
      normalForm(new [10] (new [3] (Def "" [] [3,10])))
        `shouldBe` new [0,1] (Def "" [] [1,0])
    it "can separate two binders, each overlaping half of a par" $
      normalForm(mkPar [mkNew [0,1] (mkPar [Def "P" [] [10,0,0],Def "S" [] [1,1,4]])])
        `shouldBe`
        mkPar[mkNew [0] (Def "P" [] [10,0,0]),mkNew [0] (Def "S" [] [0,0,4])]
    it "does not release binders in separating out new locations" $
      normalForm(mkNew [4,3] (mkPar [mkPar [Nil],Def "S" ["y","x"] [3,4],Nil,Nil,mkSum [(Unlocated "x",mkAbsBase (Nil)),(Located "x" 3,mkAbs 1 (Nil))],mkSum [(Unlocated "x",mkAbs 2 (Nil))]]))
        `shouldBe` (mkPar [mkNew [1] (mkPar [mkSum [(Located "x" 1,mkAbsBase (Nil)),(Unlocated "x",mkAbsBase (Nil))],mkNew [0] (Def "S" ["y","x"] [1,0])]),mkSum [(Unlocated "x",mkAbsBase (Nil))]])
    it "can unify new binding order" $
      normalForm(mkNew [1] $ mkSum [(Located "y" 1, mkAbsBase $ mkNew [0] $ mkSum [(Located "x" 0, mkAbsBase Nil)] <|> mkSum[(Located "x" 0, mkAbsBase Nil), (Located "y" 1, mkAbsBase Nil)])])
        `shouldBe`
        normalForm(mkNew [1] $ mkSum [(Located "x" 1, mkAbsBase $ mkNew [0] $ mkSum [(Located "y" 0, mkAbsBase Nil)] <|> mkSum[(Located "x" 0, mkAbsBase Nil), (Located "y" 1, mkAbsBase Nil)])])
