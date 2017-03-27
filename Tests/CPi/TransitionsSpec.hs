module CPi.TransitionsSpec (spec) where

import Test.Hspec
import CPi.AST
import CPi.Transitions
-- import Data.Map (Map)
import qualified Data.Map as M

absE :: Abstraction
absE = Abs 0 (Sum [(Located "x" 0, AbsBase Nil)])

newE :: Species
newE = New [0] (Sum [(Located "x" 0, AbsBase Nil)])

enzymeE :: Species
enzymeE = Par [Sum [(Unlocated "e", absE)]]

enzymeEnoPar :: Species
enzymeEnoPar = Sum [(Unlocated "e", absE)]

enzymeEbound :: Species
enzymeEbound = Sum [(Located "x" 0, AbsBase (Def "E" [] []))]

enzymeEdef :: Definition
enzymeEdef = SpeciesDef [] [] $ Sum [(Unlocated "e", Abs 0 enzymeEbound)]

enzymePdef :: Definition
enzymePdef = SpeciesDef [] [] $ Sum [(Unlocated "d", AbsBase (Def "P" [] []))]

enzymeSbound :: Species
enzymeSbound = (Sum [(Located "r" 0, AbsBase (Def "S" [] [])),
                     (Located "p" 0, AbsBase (Def "P" [] []))])

enzymeSdef :: Definition
enzymeSdef = SpeciesDef [] [] $ Sum [(Unlocated "s", Abs 0 enzymeSbound)]

enzymeDefs :: Env
enzymeDefs = M.fromList [("E", enzymeEdef), ("S", enzymeSdef), ("P", enzymePdef)]

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
           Abs 1 (Par [Sum [(Located "x" 1, AbsBase Nil)],
                       enzymeSnoPar]),
  enzymeES --:[Unlocated "s"]-->
           Abs 1 (Par [enzymeEnoPar,
                       Sum [(Located "r" 1, AbsBase Nil),
                            (Located "p" 1, AbsBase Nil)]]),
  enzymeES --:[Unlocated "e", Unlocated "s"]--> enzymeCPotential]

-- enzymeESFinal :: MTS
-- enzymeESFinal = simplify [
--   enzymeES ==:[Unlocated "e"]==>
--     Abs 0 (Par [Sum [(Located "x" 0, AbsBase Nil)],
--                   enzymeSnoPar]),
--   enzymeES ==:[Unlocated "s"]==>
--     Abs 0 (Par [enzymeEnoPar,
--                   Sum [(Located "r" 0, AbsBase Nil),
--                        (Located "p" 0, AbsBase Nil)]]),
--   enzymeES ==:[Unlocated "e", Unlocated "s"]==> AbsBase enzymeCFinal]
--
-- enzymeESrec :: Species
-- enzymeESrec = Def "E" [] [] <|> Def "S" [] []

enzymeCrec :: Species
enzymeCrec = new [0] (enzymeEbound <|> enzymeSbound)

-- enzymeESFinalDef :: MTS
-- enzymeESFinalDef = simplify
--   [ enzymeESrec ==:[Unlocated "e"]==> Abs 0 (enzymeEbound <|> Def "S" [] [])
--   , enzymeESrec ==:[Unlocated "s"]==> Abs 0 (Def "E" [] [] <|> enzymeSbound)
--   , enzymeESrec ==:[Unlocated "e", Unlocated "s"]==> AbsBase enzymeCrec]

enzymeCTrans :: MTS
enzymeCTrans = simplify
  [enzymeCFinal ==:[Unlocated "x", Unlocated "r"]==> AbsBase Nil,
   enzymeCFinal ==:[Unlocated "x", Unlocated "p"]==> AbsBase Nil,
   enzymeCFinal ==:[Unlocated "x"]==>
     AbsBase (new [0] (Sum [(Located "r" 0, AbsBase Nil),
                       (Located "p" 0, AbsBase Nil)])),
   enzymeCFinal ==:[Unlocated "r"]==>
     AbsBase (new [0] (Sum [(Located "x" 0, AbsBase Nil)])),
   enzymeCFinal ==:[Unlocated "p"]==>
     AbsBase (new [0] (Sum [(Located "x" 0, AbsBase Nil)]))]

enzymeCTransDef :: MTS
enzymeCTransDef = simplify
  [enzymeCrec ==:[Unlocated "x", Unlocated "r"]==> AbsBase (Def "E" [] [] <|> Def "S" [] []),
   enzymeCrec ==:[Unlocated "x", Unlocated "p"]==> AbsBase (Def "E" [] [] <|> Def "P" [] []),
   enzymeCrec ==:[Unlocated "x"]==> AbsBase (new [0] (Def "E" [] [] <|> enzymeSbound)),
   enzymeCrec ==:[Unlocated "r"]==> AbsBase (new [0] (enzymeEbound <|> Def "S" [] [])),
   enzymeCrec ==:[Unlocated "p"]==> AbsBase (new [0] (enzymeEbound <|> Def "P" [] []))]

spec :: SpecWith ()
spec = do
  describe "pretty" $ do
    it "pretty prints potential transitions" $
      pretty (enzymeE --:[Unlocated "e"]--> absE)
        `shouldBe` "e->(0)x@0->0 --{e}--> (0)x@0->0"
    it "pretty prints committed transitions" $
      pretty (enzymeE ==:[Unlocated "e"]==> AbsBase newE)
        `shouldBe` "e->(0)x@0->0 =={e}==> new 0 in x@0->0"
    it "pretty prints singleton lists of transitions" $
      (pretty [enzymeE --:[Unlocated "e"]--> absE])
        `shouldBe` "{| e->(0)x@0->0 --{e}--> (0)x@0->0 |}"
    it "pretty prints longer lists of transitions" $
      (pretty [enzymeE --:[Unlocated "e"]--> absE,
               enzymeE ==:[Unlocated "e"]==> AbsBase newE])
        `shouldBe` "{| e->(0)x@0->0 --{e}--> (0)x@0->0,\n"
                ++ "   e->(0)x@0->0 =={e}==> new 0 in x@0->0 |}"
  describe "simplify" $ do
    it "allows us to compares two multisets of the same transitions in a different order" $
      shouldBe
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE ==:[Unlocated "e"]==> AbsBase newE])
        (simplify [enzymeE ==:[Unlocated "e"]==> AbsBase newE,
                   enzymeE --:[Unlocated "e"]--> absE])
    it "allows us to compare two multisets of the same transitions with repeated transitions" $
      shouldBe
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE ==:[Unlocated "e"]==> AbsBase newE])
        (simplify [enzymeE ==:[Unlocated "e"]==> AbsBase newE,
                   enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> absE])
    it "allows us to can tell multisets with different multiplicities are not equal" $
      shouldNotBe
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE ==:[Unlocated "e"]==> AbsBase newE])
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE ==:[Unlocated "e"]==> AbsBase newE])
  describe "union" $ do
    it "takes the union of two mts" $
      shouldBe
        (simplify $ (++)
          ([enzymeE --:[Unlocated "e"]--> absE,
            enzymeE ==:[Unlocated "e"]==> AbsBase newE])
          ([enzymeE --:[Unlocated "e"]--> absE]))
        (simplify $ [enzymeE --:[Unlocated "e"]--> absE,
                     enzymeE --:[Unlocated "e"]--> absE,
                     enzymeE ==:[Unlocated "e"]==> AbsBase newE])
  describe "trans" $ do
    it "finds E potential transitions" $
      shouldBe
        (simplify $ potentialTrans $ trans enzymeE M.empty)
        (simplify [enzymeE --:[Unlocated "e"]--> absE])
    it "finds E potential transitions without par" $
      shouldBe
        (potentialTrans $ trans enzymeEnoPar M.empty)
        (simplify [enzymeEnoPar --:[Unlocated "e"]--> absE])
    it "finds E potential transitions recursive" $
      shouldBe
        (simplify $ potentialTrans $ trans (specBody enzymeEdef) enzymeDefs)
        (simplify [specBody enzymeEdef --:[Unlocated "e"]--> Abs 0 (enzymeEbound)])
    it "finds E potential transitions from def" $
      shouldBe
        (simplify $ potentialTrans $ trans (Def "E" [] []) enzymeDefs)
        (simplify [(Def "E" [] []) --:[Unlocated "e"]--> Abs 0 (enzymeEbound)])
    it "finds S potential transitions" $
      shouldBe
        (simplify $ potentialTrans $ trans enzymeS M.empty)
        (simplify [enzymeS --:[Unlocated "s"]--> absS])
    it "finds S potential transitions without par" $
      shouldBe
        (simplify $ potentialTrans $ trans enzymeSnoPar M.empty)
        (simplify [enzymeSnoPar --:[Unlocated "s"]--> absS])
    it "finds S potential transitions recursive" $
      shouldBe
        (simplify $ potentialTrans $ trans (specBody enzymeSdef) enzymeDefs)
        (simplify [specBody enzymeSdef --:[Unlocated "s"]--> Abs 0 (enzymeSbound)])
    it "finds S potential transitions from def" $
      shouldBe
        (simplify $ potentialTrans $ trans (Def "S" [] []) enzymeDefs)
        (simplify [(Def "S" [] []) --:[Unlocated "s"]--> Abs 0 (enzymeSbound)])
    it "finds E|S potential transitions without par" $
      (simplify $ potentialTrans $ trans enzymeES M.empty)
        `shouldBe` simplify enzymeESPotential
    it "finds E|S potential transitions without par, upto pretty printing" $
      shouldBe
        (pretty $ simplify $ potentialTrans $ trans enzymeES M.empty)
        (pretty $ simplify $ enzymeESPotential)
    -- should transF even exist any more?
    -- it "finds E|S final transitions without par" $
    --   (simplify $ transF enzymeES M.empty) `shouldBe` enzymeESFinal
    -- it "finds E|S final transitions without par, upto pretty printing" $
    --     (pretty $ simplify $ transF enzymeES M.empty) `shouldBe` pretty enzymeESFinal
    -- it "finds E|S final transitions when calling by name" $
    --   simplify (transF enzymeESrec enzymeDefs) `shouldBe` enzymeESFinalDef
    it "finds C final transitions, upto pretty printing" $
      (pretty $ simplify $ transF enzymeCFinal M.empty) `shouldBe` pretty enzymeCTrans
    it "finds C final transitions" $
      simplify (transF enzymeCFinal M.empty) `shouldBe` enzymeCTrans
    it "finds C final transitions with recursive definitions" $
      simplify (transF enzymeCrec enzymeDefs) `shouldBe` enzymeCTransDef
