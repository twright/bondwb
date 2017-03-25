module CPi.ASTSpec (spec) where

import Test.Hspec
import CPi.AST

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
          Sum [(Unlocated "s", Abs 0 $ Sum [(Located "z" 2,
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
  describe "colocate" $ do
    it "can place two simple abstractions at the same location" $
      colocate absA absB `shouldBe` absAB
    it "does not mess up parallel composition" $
      ((absA `colocate` absB) `colocate` absB) `shouldBe` absABB
  describe "simplify" $ do
    it "removes unused binders" $ do
      shouldBe
        (simplify $ New [1] $ Sum [(Located "x" 0, AbsBase Nil)])
        (Sum [(Located "x" 0, AbsBase Nil)])
    it "keeps used binders" $ do
      shouldBe
        (simplify $ New [0] $ Sum [(Located "x" 0, AbsBase Nil)])
        (New [0] $ Sum [(Located "x" 0, AbsBase Nil)])
  describe "normalForm" $ do
    it "removes unused binders" $ do
      shouldBe
        (normalForm $ New [1] $ Sum [(Located "x" 0, AbsBase Nil)])
        (Sum [(Located "x" 0, AbsBase Nil)])
    it "keeps used binders" $ do
      shouldBe
        (normalForm $ New [0] $ Sum [(Located "x" 0, AbsBase Nil)])
        (New [0] $ Sum [(Located "x" 0, AbsBase Nil)])
