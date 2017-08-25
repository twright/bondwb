module CPi.TransitionsSpec (spec) where

import Test.Hspec
import CPi.AST
import CPi.Transitions
import CPi.Base
-- import Data.Map (Map)
import qualified Data.Map as M

absE :: Abstraction
absE = mkAbs 0 (mkSum [(Located "x" 0, mkAbsBase Nil)])

newE :: Species
newE = new [0] (mkSum [(Located "x" 0, mkAbsBase Nil)])

enzymeE :: Species
enzymeE = mkPar [mkSum [(Unlocated "e", absE)]]

enzymeEnoPar :: Species
enzymeEnoPar = mkSum [(Unlocated "e", absE)]

enzymeEbound :: Species
enzymeEbound = mkSum [(Located "x" 0, mkAbsBase (Def "E" [] []))]

enzymeEdef :: SpeciesDefinition
enzymeEdef = SpeciesDef [] [] $ mkSum [(Unlocated "e", mkAbs 0 enzymeEbound)]

enzymePdef :: SpeciesDefinition
enzymePdef = SpeciesDef [] [] $ mkSum [(Unlocated "d", mkAbsBase (Def "P" [] []))]

enzymeSbound :: Species
enzymeSbound = mkSum [(Located "r" 0, mkAbsBase (Def "S" [] [])),
                     (Located "p" 0, mkAbsBase (Def "P" [] []))]

enzymeSdef :: SpeciesDefinition
enzymeSdef = SpeciesDef [] [] $ mkSum [(Unlocated "s", mkAbs 0 enzymeSbound)]

enzymeDefs :: Env
enzymeDefs = M.fromList [("E", enzymeEdef), ("S", enzymeSdef), ("P", enzymePdef)]

absS :: Abstraction
absS = mkAbs 0 (mkSum [(Located "r" 0, mkAbsBase Nil),
                   (Located "p" 0, mkAbsBase Nil)])

enzymeS :: Species
enzymeS = mkPar [mkSum [(Unlocated "s", absS)]]

enzymeSnoPar :: Species
enzymeSnoPar = mkSum [(Unlocated "s", absS)]

enzymeCFinal :: Species
enzymeCFinal = new [0] (mkPar [mkSum [(Located "x" 0, mkAbsBase Nil)],
                               mkSum [(Located "r" 0, mkAbsBase Nil),
                                      (Located "p" 0, mkAbsBase Nil)]])

enzymeES :: Species
enzymeES = mkPar [enzymeEnoPar, enzymeSnoPar]

enzymeESTrans :: MTS
enzymeESTrans = simplify [
  enzymeES --:[Unlocated "e"]-->
           mkAbs 1 (mkPar [mkSum [(Located "x" 1, mkAbsBase Nil)],
                       enzymeSnoPar]),
  enzymeES --:[Unlocated "s"]-->
           mkAbs 1 (mkPar [enzymeEnoPar,
                       mkSum [(Located "r" 1, mkAbsBase Nil),
                            (Located "p" 1, mkAbsBase Nil)]])]

enzymeCrec :: Species
enzymeCrec = new [0] (enzymeEbound <|> enzymeSbound)

enzymeCTrans :: MTS
enzymeCTrans = simplify
  [enzymeCFinal --:[Unlocated "x", Unlocated "r"]--> mkAbsBase Nil,
   enzymeCFinal --:[Unlocated "x", Unlocated "p"]--> mkAbsBase Nil,
   enzymeCFinal --:[Unlocated "x"]-->
     mkAbsBase (new [0] (mkSum [(Located "r" 0, mkAbsBase Nil),
                       (Located "p" 0, mkAbsBase Nil)])),
   enzymeCFinal --:[Unlocated "r"]-->
     mkAbsBase (new [0] (mkSum [(Located "x" 0, mkAbsBase Nil)])),
   enzymeCFinal --:[Unlocated "p"]-->
     mkAbsBase (new [0] (mkSum [(Located "x" 0, mkAbsBase Nil)]))]

enzymeCTransDef :: MTS
enzymeCTransDef = simplify
  [enzymeCrec --:[Unlocated "x", Unlocated "r"]--> mkAbsBase (Def "E" [] [] <|> Def "S" [] []),
   enzymeCrec --:[Unlocated "x", Unlocated "p"]--> mkAbsBase (Def "E" [] [] <|> Def "P" [] []),
   enzymeCrec --:[Unlocated "x"]--> mkAbsBase (new [0] (Def "E" [] [] <|> enzymeSbound)),
   enzymeCrec --:[Unlocated "r"]--> mkAbsBase (new [0] (enzymeEbound <|> Def "S" [] [])),
   enzymeCrec --:[Unlocated "p"]--> mkAbsBase (new [0] (enzymeEbound <|> Def "P" [] []))]

spec :: SpecWith ()
spec = do
  describe "pretty" $ do
    it "pretty prints potential transitions" $
      pretty (enzymeE --:[Unlocated "e"]--> absE)
        `shouldBe` "e->(0)x@0->0 --{e}--> (0)x@0->0"
    it "pretty prints committed transitions" $
      pretty (enzymeE --:[Unlocated "e"]--> mkAbsBase newE)
        `shouldBe` "e->(0)x@0->0 --{e}--> new 0 in x@0->0"
    it "pretty prints singleton lists of transitions" $
      pretty [enzymeE --:[Unlocated "e"]--> absE]
        `shouldBe` "{| e->(0)x@0->0 --{e}--> (0)x@0->0 |}"
    it "pretty prints longer lists of transitions" $
      pretty [enzymeE --:[Unlocated "e"]--> absE,
              enzymeE --:[Unlocated "e"]--> mkAbsBase newE]
        `shouldBe` "{| e->(0)x@0->0 --{e}--> (0)x@0->0,\n"
                ++ "   e->(0)x@0->0 --{e}--> new 0 in x@0->0 |}"
  describe "simplify" $ do
    it "allows us to compares two multisets of the same transitions in a different order" $
      shouldBe
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> mkAbsBase newE])
        (simplify [enzymeE --:[Unlocated "e"]--> mkAbsBase newE,
                   enzymeE --:[Unlocated "e"]--> absE])
    it "allows us to compare two multisets of the same transitions with repeated transitions" $
      shouldBe
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> mkAbsBase newE])
        (simplify [enzymeE --:[Unlocated "e"]--> mkAbsBase newE,
                   enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> absE])
    it "allows us to can tell multisets with different multiplicities are not equal" $
      shouldNotBe
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> mkAbsBase newE])
        (simplify [enzymeE --:[Unlocated "e"]--> absE,
                   enzymeE --:[Unlocated "e"]--> mkAbsBase newE])
  describe "trans" $ do
    it "finds E potential transitions" $
      shouldBe
        (simplify $ trans M.empty enzymeE)
        (simplify [enzymeE --:[Unlocated "e"]--> absE])
    it "finds E potential transitions without par" $
      shouldBe
        (simplify $ trans M.empty enzymeEnoPar)
        (simplify [enzymeEnoPar --:[Unlocated "e"]--> absE])
    it "finds E potential transitions recursive" $
      shouldBe
        (simplify $ trans enzymeDefs (specBody enzymeEdef))
        (simplify [specBody enzymeEdef --:[Unlocated "e"]-->
                   mkAbs 0 enzymeEbound])
    it "finds E potential transitions from def" $
      shouldBe
        (simplify $ trans enzymeDefs (Def "E" [] []))
        (simplify [Def "E" [] [] --:[Unlocated "e"]-->
                   mkAbs 0 enzymeEbound])
    it "finds S potential transitions" $
      shouldBe
        (simplify $ trans M.empty enzymeS)
        (simplify [enzymeS --:[Unlocated "s"]--> absS])
    it "finds S potential transitions without par" $
      shouldBe
        (simplify $ trans M.empty enzymeSnoPar)
        (simplify [enzymeSnoPar --:[Unlocated "s"]--> absS])
    it "finds S potential transitions recursive" $
      shouldBe
        (simplify $ trans enzymeDefs (specBody enzymeSdef))
        (simplify [specBody enzymeSdef --:[Unlocated "s"]--> mkAbs 0 enzymeSbound])
    it "finds S potential transitions from def" $
      shouldBe
        (simplify $ trans enzymeDefs (Def "S" [] []))
        (simplify [Def "S" [] [] --:[Unlocated "s"]--> mkAbs 0 enzymeSbound])
    it "finds E|S potential transitions without par" $
      simplify (trans M.empty enzymeES) `shouldBe` simplify enzymeESTrans
    it "finds E|S transitions without par, upto pretty printing" $
      shouldBe
        (pretty $ simplify $ trans M.empty enzymeES)
        (pretty $ simplify enzymeESTrans)
    it "finds C transitions, upto pretty printing" $
      pretty (simplify $ transF M.empty enzymeCFinal)
        `shouldBe` pretty (simplify enzymeCTrans)
    it "finds C finalized transitions with recursive definitions" $
      simplify (transF enzymeDefs enzymeCrec) `shouldBe` enzymeCTransDef
    it "finds all transitions with 3 sites upto pretty" $
      pretty (simplify (trans M.empty (mkSum [(Located "a" 0, mkAbsBase Nil)]
                           <|> mkSum [(Located "b" 0, mkAbsBase Nil)]
                           <|> mkSum [(Located "c" 0, mkAbsBase Nil)])))
        `shouldBe`
        "{| a@0->0 | c@0->0 | b@0->0 --{a@0,b@0,c@0}--> 0,\n"
     ++ "   a@0->0 | c@0->0 | b@0->0 --{a@0,b@0}--> c@0->0,\n"
     ++ "   a@0->0 | c@0->0 | b@0->0 --{a@0,c@0}--> b@0->0,\n"
     ++ "   a@0->0 | c@0->0 | b@0->0 --{a@0}--> c@0->0 | b@0->0,\n"
     ++ "   a@0->0 | c@0->0 | b@0->0 --{b@0,c@0}--> a@0->0,\n"
     ++ "   a@0->0 | c@0->0 | b@0->0 --{b@0}--> a@0->0 | c@0->0,\n"
     ++ "   a@0->0 | c@0->0 | b@0->0 --{c@0}--> a@0->0 | b@0->0 |}"
