module CPi.TransitionsSpec (spec) where

import Test.Hspec
import CPi.AST
import CPi.Transitions

absE :: Abstraction
absE = Abs 0 (Sum [(Located "x" 0, AbsBase Nil)])

newE :: Species
newE = New [0] (Sum [(Located "x" 0, AbsBase Nil)])

enzymeE :: Species
enzymeE = Par [Sum [(Unlocated "e", absE)]]

enzymeEnoPar :: Species
enzymeEnoPar = Sum [(Unlocated "e", absE)]

absS :: Abstraction
absS = Abs 0 (Sum [(Located "r" 0, AbsBase Nil),
                   (Located "p" 0, AbsBase Nil)])

enzymeS :: Species
enzymeS = Par [Sum [(Unlocated "s", absS)]]

enzymeSnoPar :: Species
enzymeSnoPar = Sum [(Unlocated "s", absS)]

enzymeCPotential :: Abstraction
enzymeCPotential = Abs 0 (Par [Sum [(Located "x" 0, AbsBase Nil)],
                               Sum [(Located "r" 0, AbsBase Nil),
                                    (Located "p" 0, AbsBase Nil)]])

enzymeCFinal :: Species
enzymeCFinal = New [0] (Par [Sum [(Located "x" 0, AbsBase Nil)],
                             Sum [(Located "r" 0, AbsBase Nil),
                                  (Located "p" 0, AbsBase Nil)]])
-- enzymeEC :: Abstraction
-- enzymeEC = AbsPar [AbsBase enzymeE, enzymeC]

enzymeES :: Species
enzymeES = Par [enzymeEnoPar, enzymeSnoPar]

enzymeESPotential :: MTS
enzymeESPotential = simplify [
  enzymeES --:[Unlocated "e"]-->
           Abs 0 (Par [Sum [(Located "x" 0, AbsBase Nil)],
                       enzymeSnoPar]),
  enzymeES --:[Unlocated "s"]-->
           Abs 0 (Par [enzymeEnoPar,
                       Sum [(Located "r" 0, AbsBase Nil),
                            (Located "p" 0, AbsBase Nil)]]),
  enzymeES --:[Unlocated "e", Unlocated "s"]--> enzymeCPotential]

enzymeESFinal :: MTS
enzymeESFinal = simplify [
  enzymeES ==:[Unlocated "e"]==>
    New [0] (Par [Sum [(Located "x" 0, AbsBase Nil)],
                  enzymeSnoPar]),
  enzymeES ==:[Unlocated "s"]==>
    New [0] (Par [enzymeEnoPar,
                  Sum [(Located "r" 0, AbsBase Nil),
                       (Located "p" 0, AbsBase Nil)]]),
  enzymeES ==:[Unlocated "e", Unlocated "s"]==> enzymeCFinal]

enzymeCTrans :: MTS
enzymeCTrans = simplify
  [enzymeCFinal ==:[Unlocated "x", Unlocated "r"]==> Nil,
   enzymeCFinal ==:[Unlocated "x", Unlocated "p"]==> Nil,
   enzymeCFinal ==:[Unlocated "x"]==>
     new [0] (Sum [(Located "r" 0, AbsBase Nil),
                   (Located "p" 0, AbsBase Nil)]),
   enzymeCFinal ==:[Unlocated "r"]==>
     new [0] (Sum [(Located "x" 0, AbsBase Nil)]),
   enzymeCFinal ==:[Unlocated "p"]==>
     new [0] (Sum [(Located "x" 0, AbsBase Nil)])]

spec :: SpecWith ()
spec = do
  describe "pretty" $ do
    it "pretty prints potential transitions" $
      pretty (Trans enzymeE [Unlocated "e"] absE)
        `shouldBe` "e->(0)x@0->0 --{e}--> (0)x@0->0"
    it "pretty prints committed transitions" $
      pretty (TransF enzymeE [Unlocated "e"] newE)
        `shouldBe` "e->(0)x@0->0 =={e}==> new 0 in x@0->0"
    it "pretty prints singleton lists of transitions" $
      (pretty [Trans enzymeE [Unlocated "e"] absE])
        `shouldBe` "{| e->(0)x@0->0 --{e}--> (0)x@0->0 |}"
    it "pretty prints longer lists of transitions" $
      (pretty [Trans enzymeE [Unlocated "e"] absE,
               TransF enzymeE [Unlocated "e"] newE])
        `shouldBe` "{| e->(0)x@0->0 --{e}--> (0)x@0->0,\n"
                ++ "   e->(0)x@0->0 =={e}==> new 0 in x@0->0 |}"
  describe "simplify" $ do
    it "allows us to compares two multisets of the same transitions in a different order" $
      shouldBe
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE ==:[Unlocated "e"]==> newE])
        (simplify [enzymeE ==:[Unlocated "e"]==> newE,
                   enzymeE --:[Unlocated "e"]--> absE])
    it "allows us to compare two multisets of the same transitions with repeated transitions" $
      shouldBe
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE ==:[Unlocated "e"]==> newE])
        (simplify [enzymeE ==:[Unlocated "e"]==> newE,
                   enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> absE])
    it "allows us to can tell multisets with different multiplicities are not equal" $
      shouldNotBe
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE ==:[Unlocated "e"]==> newE])
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE ==:[Unlocated "e"]==> newE])
  describe "union" $ do
    it "takes the union of two mts" $
      shouldBe
        (simplify $ (++)
          ([enzymeE --:[Unlocated "e"]--> absE,
            enzymeE ==:[Unlocated "e"]==> newE])
          ([enzymeE --:[Unlocated "e"]--> absE]))
        (simplify $ [enzymeE --:[Unlocated "e"]--> absE,
                     enzymeE --:[Unlocated "e"]--> absE,
                     enzymeE ==:[Unlocated "e"]==> newE])
  describe "trans" $ do
    it "finds E potential transitions" $
      shouldBe
        (simplify $ potentialTrans $ trans enzymeE)
        (simplify [enzymeE --:[Unlocated "e"]--> absE]) 
    it "finds E potential transitions without par" $
      shouldBe
        (potentialTrans $ trans enzymeEnoPar)
        (simplify [enzymeEnoPar --:[Unlocated "e"]--> absE]) 
    it "finds S potential transitions" $
      shouldBe
        (simplify $ potentialTrans $ trans enzymeS)
        (simplify [enzymeS --:[Unlocated "s"]--> absS]) 
    it "finds S potential transitions without par" $
      shouldBe
        (simplify $ potentialTrans $ trans enzymeSnoPar)
        (simplify [enzymeSnoPar --:[Unlocated "s"]--> absS]) 
    it "finds E|S potential transitions without par" $
      (simplify $ potentialTrans $ trans enzymeES) `shouldBe` enzymeESPotential
    it "finds E|S potential transitions without par, upto pretty printing" $
      shouldBe
        (pretty $ simplify $ potentialTrans $ trans enzymeES)
        (pretty $ simplify $ enzymeESPotential)
    it "finds E|S final transitions without par" $
      (simplify $ transF enzymeES) `shouldBe` enzymeESFinal
    it "finds E|S final transitions without par, upto pretty printing" $
        (pretty $ simplify $ transF enzymeES) `shouldBe` pretty enzymeESFinal
    it "finds C final transitions, upto pretty printing" $
      (pretty $ simplify $ transF enzymeCFinal) `shouldBe` pretty enzymeCTrans
    it "finds C final transitions" $
      simplify (transF enzymeCFinal) `shouldBe` enzymeCTrans
