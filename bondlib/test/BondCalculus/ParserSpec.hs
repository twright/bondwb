module BondCalculus.ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Test.QuickCheck
import BondCalculus.AST
import BondCalculus.Parser
import BondCalculus.Base
import Data.Hashable (hash)
import qualified BondCalculus.Examples as EX
import BondCalculus.Examples (rabbitSource)
import BondCalculus.Symbolic
import Data.Either

-- a `shouldParseNf` b = a `parseSatisfies` (\p -> normalForm p == normalForm b)
-- shouldParseModel :: Parser (BondCalculusModel Double) -> BondCalculusModel a

a `shouldParseModel` b = a
                         `parseSatisfies`
                         (\m -> speciesDefs m == speciesDefs b
                             && affinityNetworkDefs m == affinityNetworkDefs b
                             && processDefs m == processDefs b)
a `shouldParseSym` b = a `shouldParse` (b :: SymbolicExpr Double)

rabbitModel :: BondCalculusModel Double
rabbitModel = EX.rabbitModel

spec :: SpecWith ()
spec = do
  describe "prefix" $ do
    it "should parse a single letter unlocated prefix" $
      parse prefix "" "x" `shouldParse` Unlocated "x"
    it "should parse a multiletter unlocated prefix" $
      parse prefix "" "eat" `shouldParse` Unlocated "eat"
    it "should parse a located prefix" $
      parse prefix "" "x@l" `shouldParse` Located "x" (fromIntegral $ hash "l")
    it "should parse a located prefix, using a number as the location" $
      parse prefix "" "x@0" `shouldParse` Located "x" (fromIntegral $ hash "0")
  describe "nil" $ do
    it "should parse nils" $
      parse nil "" "0" `shouldParse` Nil
  describe "guard" $ do
    it "should parse guarded prefixes with no binding" $
      parse prefixGuard "" "x->0" `shouldParse` (Unlocated "x", mkAbsBase Nil)
    it "should parse guarded prefixes with binding" $
      parse prefixGuard "" "x(l)->0"
        `shouldParse` (Unlocated "x", mkAbs (fromIntegral $ hash "l") Nil)
    it "should parse guarded prefixes with binding after arrow" $
      parse prefixGuard "" "x->(l)0"
        `shouldParse` (Unlocated "x", mkAbs (fromIntegral $ hash "l") Nil)
    it "should parse guarded prefixes with binding, spaced out" $
      parse prefixGuard "" "x(l) -> 0"
        `shouldParse` (Unlocated "x", mkAbs (fromIntegral $ hash "l") Nil)
    it "should parse guarded prefixes using dot" $
      parse prefixGuard "" "x.0" `shouldParse` (Unlocated "x", mkAbsBase Nil)
    it "should parse guard with located prefix" $
      parse prefixGuard "" "x@l -> 0"
        `shouldParse` (Located "x" (fromIntegral $ hash "l"), mkAbsBase Nil)
    it "should parse prefix guarded species with no species" $
      parse prefixGuard "" "x" `shouldParse` (Unlocated "x", mkAbsBase Nil)
    it "should parse a guard with a sum" $
      parse prefixGuard "" "x -> (y + z)"
        `shouldParse`
        (Unlocated "x",
          mkAbsBase $ mkSum [(Unlocated "y", mkAbsBase Nil),
                             (Unlocated "z", mkAbsBase Nil)])
    it "should parse a string of guards, without needing brackets" $
      parse prefixGuard "" "x -> y -> z"
        `shouldParse`
        (Unlocated "x", mkAbsBase
            $ mkSum [(Unlocated "y", mkAbsBase
                    $ mkSum [(Unlocated "z", mkAbsBase Nil)])])
  describe "guardedSum" $ do
    it "should parse a sum of two guards" $
      parse guardedSum "" "x@l -> 0 + y(m) -> 0"
        `shouldParse`
        mkSum [(Located "x" (fromIntegral $ hash "l"), mkAbsBase Nil),
               (Unlocated "y", mkAbs (fromIntegral $ hash "m") Nil)]
    it "should parse a sum with a single guard" $
      parse guardedSum "" "x@l -> 0"
        `shouldParse`
        mkSum [(Located "x" (fromIntegral $ hash "l"), mkAbsBase Nil)]
  describe "def" $ do
    it "should parse an application of a definiton with no arguments" $
      parse def "" "E" `shouldParse` Def "E" [] []
    it "should parse an application of a definition with a single argument and location" $
      parse def "" "E(x;l)" `shouldParse` Def "E" ["x"] [fromIntegral $ hash "l"]
    it "should parse an application of a definition with multiple arguments and locations" $
      parse def "" "E(x,y;l,m)"
        `shouldParse`
        Def "E" ["x", "y"] [fromIntegral $ hash "l",
                            fromIntegral $ hash "m"]
    it "should parse an application of a definition with multiple arguments and locations, with spaces in between args" $
      parse def "" "E(x, y; l, m)"
        `shouldParse`
        Def "E" ["x", "y"] [fromIntegral $ hash "l",
                            fromIntegral $ hash "m"]
    it "should parse an application of a definition with multiple arguments and locations, with spaces and comments in between args" $
      parse def "" "E(x {- arg x -}, y {- arg y -}; l {- loc l -}, m {- loc m -}) {- application of E -}"
        `shouldParse`
        Def "E" ["x", "y"] [fromIntegral $ hash "l",
                            fromIntegral $ hash "m"]
    it "should parse an application of a definition with only arguments, and no locations" $
      parse def "" "E(x,y;)" `shouldParse` Def "E" ["x", "y"] []
    it "should parse an application of a definition with only locations, and no args" $
      parse def "" "E(;l,m)"
        `shouldParse` Def "E" [] [fromIntegral $ hash "l",
                                  fromIntegral $ hash "m"]
  describe "restriction" $ do
    it "should restrict a location in a species" $
      let l = fromIntegral (hash "l")
      in parse restriction "" "new l in x@l"
         `shouldParse` mkNew [l] (mkSum [(Located "x" l, mkAbsBase Nil)])
    it "should restrict multiple locations in a species" $
      let l = fromIntegral $ hash "l"
          m = fromIntegral $ hash "m"
      in parse restriction "" "new l,m in x@l + y@m"
         `shouldParse` mkNew [l,m] (mkSum [(Located "x" l, mkAbsBase Nil),
                                           (Located "y" m, mkAbsBase Nil)])
  describe "species" $ do
    it "should parse a parallel composition of nils" $
      parse species "" "0 | 0" `shouldParse` mkPar [Nil, Nil]
    it "should parse C from enzyme with explicit complexes" $
      parse species "" "r->(S|E) + p->P"
        `shouldParse`
        mkSum [(Unlocated "r", mkAbsBase $ mkPar [Def "S" [] [],
                                                  Def "E" [] []]),
               (Unlocated "p", mkAbsBase $ Def "P" [] [])]
    it "should parse C from enzyme with dynamic complex formation" $
      let l = fromIntegral $ hash "l"
      in parse species "" "new l in x@l->E | (r@l->S + p@l->P)"
         `shouldParse`
         mkNew [l] (mkPar [mkSum [(Located "x" l, mkAbsBase $ Def "E" [] [])],
                           mkSum [(Located "r" l, mkAbsBase $ Def "S" [] []),
                                  (Located "p" l, mkAbsBase $ Def "P" [] [])]])
    it "should parse a strand from the genetic XOR gate example" $
      let l = fromIntegral $ hash "l"
      in parse species "" "new l in Unbound(bindA, unbindA, boundA, unboundA; l) | Unbound(bindB, unbindB, boundB, unboundB; l) | Transcriber(;l)"
         `shouldParse`
         mkNew [l]
         (mkPar [Def "Unbound" ["bindA", "unbindA", "boundA", "unboundA"] [l],
                 Def "Unbound" ["bindB", "unbindB", "boundB", "unboundB"] [l],
                 Def "Transcriber" [] [l]])
    it "should parse an unbound site from the genetic XOR gate example" $
      let l = fromIntegral $ hash "l"
          m = fromIntegral $ hash "m"
      in parse species "" "bind(m) -> Bound(bind, unbind, bound, unbound; l, m) + unbound@l -> Unbound(bind, unbind, bound, unbound; l)"
         `shouldParse`
         mkSum [(Unlocated "bind",
                 mkAbs m $ Def "Bound"
                               ["bind", "unbind", "bound", "unbound"] [l, m]),
                (Located "unbound" l,
                 mkAbsBase $ Def "Unbound"
                                 ["bind", "unbind", "bound", "unbound"] [l])]
    it "should be the left inverse of pretty printing, upto normal form" $
      property (\x -> let converted = normalForm $ relocateAll flocs hlocs x
                          flocs     = freeLocs x
                          hlocs     = map (fromIntegral . hash . show) flocs
                      in fmap normalForm (parse species "" $ pretty x)
                         === Right converted)
  describe "speciesDef" $ do
    it "allows us to define the transcription factor from the genetic XOR gate model" $
      let m = fromIntegral $ hash "m"
      in  parse speciesDef "" "species TranscriptionFactor(bind, unbind;) = bind(m) -> unbind@m -> TranscriptionFactor(bind, unbind;);"
          `shouldParse`
          ("TranscriptionFactor",
           SpeciesDef ["bind", "unbind"]
                      []
                      (mkSum [(Unlocated "bind",
                               mkAbs m $ mkSum [(Located "unbind" m,
                                                 mkAbsBase
                                               $ Def "TranscriptionFactor"
                                                     ["bind", "unbind"] [])])]))
    it "should parse an unbound site species definition from the genetic XOR gate example" $
      let l = fromIntegral $ hash "l"
          m = fromIntegral $ hash "m"
      in parse speciesDef "" "species Unbound(bind, unbind, bound, unbound; l) = bind(m) -> Bound(bind, unbind, bound, unbound; l, m) + unbound@l -> Unbound(bind, unbind, bound, unbound; l);"
         `shouldParse`
         ("Unbound",
          SpeciesDef ["bind", "unbind", "bound", "unbound"] [l]
            $ mkSum [(Unlocated "bind",
                      mkAbs m $ Def "Bound"
                                    ["bind", "unbind", "bound", "unbound"]
                                    [l, m]),
                     (Located "unbound" l,
                      mkAbsBase $ Def "Unbound"
                                  ["bind", "unbind", "bound", "unbound"] [l])])
  describe "affinityNetwork" $ do
    it "should parse an affinity network application" $
      parse affinityNetwork "" "Aff(1, 2, 3)"
        `shouldParse` AffinityNetworkAppl "Aff" [1.0 :: Double, 2.0, 3.0]
    it "should parse an affinity network literal" $
      parse affinityNetwork "" "{ e || s at rate MA(1); }"
        `shouldParse` AffinityNetworkSpec [Affinity
                                           (RateLawAppl
                                            "MA"
                                            [RateLawParamVal (1.0 :: Double)])
                                           [["e"], ["s"]]]
    it "should parse an affinity network literal with an interval rate" $
      parse affinityNetwork "" "{ e || s at rate MA([2.0 .. 3.5]); }"
        `shouldParse` AffinityNetworkSpec [Affinity
                                           (RateLawAppl
                                            "MA"
                                            [RateLawParamVal
                                             (fromEndpoints 2.0 3.5)])
                                           [["e"], ["s"]]]
  describe "affinityNetworkDef" $ do
    it "should parse the affinity network definition for a genetic XOR gate" $
      parse affinityNetworkDef ""
       ("affinity network M(k2, k3, l2, l3) = {\n"
     ++ "  transcribe + boundA + unboundB, "
       ++ "transcribe + unboundA + boundB at rate L(1.0);\n"
     ++ "  bindA, cobindA at rate MA(k2);\n"
     ++ "  unbindA + counbindA at rate MA(l2);\n"
     ++ "}")
      `shouldParse`
      ( "M"
      , AffinityNetworkDef ["k2", "k3", "l2", "l3"]
        [ Affinity (RateLawAppl "L" [RateLawParamVal 1.0])
                   [ ["transcribe", "boundA", "unboundB"]
                   , ["transcribe", "unboundA", "boundB"] ]
        , Affinity (RateLawAppl "MA" [RateLawParamVar "k2"])
                   [["bindA"], ["cobindA"]]
        , Affinity (RateLawAppl "MA" [RateLawParamVar "l2"])
                   [["unbindA", "counbindA"]]]
         :: AffinityNetworkDefinition Double)
  describe "processDef" $ do
    it "should parse the process definition for a genetic XOR gate" $
      parse processDef ""
        ("process GeneticXORGate = [1.0] Strand\n"
      ++ "  || [0.1] TranscriptionFactor(cobindA, counbindA)\n"
      ++ "  || [0.1] TranscriptionFactor(cobindB, counbindB)\n"
      ++ "  || [0.0] Product\n"
      ++ "  with network M(0.5,0.5,0.1,0.1);")
      `shouldParse`
      ( "GeneticXORGate"
      , Process (AffinityNetworkAppl "M" [0.5, 0.5, 0.1, 0.1])
                [ (1.0, Def "Strand" [] [])
                , (0.1, Def "TranscriptionFactor" ["cobindA", "counbindA"] [])
                , (0.1, Def "TranscriptionFactor" ["cobindB", "counbindB"] [])
                , (0.0, Def "Product" [] []) ] :: AbstractProcess Double )
  describe "processComponents" $ do
    it "should parse a single double process component" $
      parse processComponent "" "[1.0] Strand"
        `shouldParse`
        (1.0 :: Double, Def "Strand" [] [])
    it "should parse a single trivial interval process component" $
      parse processComponent "" "[1.0] Strand"
        `shouldParse`
        (1.0 :: Interval, Def "Strand" [] [])
    it "should parse a single trivial interval process component" $
      parse processComponent "" "[1.0 .. 2.5] Strand"
        `shouldParse`
        (fromEndpoints 1.0 2.5 :: Interval, Def "Strand" [] [])
  describe "process" $ do
    it "should parse the process expression for a genetic XOR gate" $
      parse process ""
        ("[1.0] Strand"
      ++ " || [0.1] TranscriptionFactor(cobindA, counbindA)"
      ++ " || [0.1] TranscriptionFactor(cobindB, counbindB)"
      ++ " || [0.0] Product")
      `shouldParse`
      (mkProcess mempty
                 [ (1.0, Def "Strand" [] [])
                 , (0.1, Def "TranscriptionFactor" ["cobindA", "counbindA"] [])
                 , (0.1, Def "TranscriptionFactor" ["cobindB", "counbindB"] [])
                 , (0.0, Def "Product" [] []) ] :: AbstractProcess Double)
    it ("should parse the process expression for a genetic XOR gate " ++
        "with an interval concentration") $
      fmap show (parse (process :: Parser (AbstractProcess Interval)) ""
        ("[1.0] Strand"
      ++ " || [0.1, 0.5] TranscriptionFactor(cobindA, counbindA)"
      ++ " || [0.2 .. 0.7] TranscriptionFactor(cobindB, counbindB)"
      ++ " || [0.0] Product"))
      `shouldParse`
        ("Process (AffinityNetworkSpec []) [([0.100000 .. 0.500000],"++
         "Def \"TranscriptionFactor\" [\"cobindA\",\"counbindA\"] []),"++
         "([1.000000 .. 1.000000],Def \"Strand\" [] []),([0.000000 .. 0.000000],"++
         "Def \"Product\" [] []),([0.200000 .. 0.700000],Def \"TranscriptionFactor\""++
         " [\"cobindB\",\"counbindB\"] [])]")
    -- We use pretty printing to check equality due to having no better way
    -- of checking approximate equality of floats
    --   (mkProcess mempty
    --              [ (fromFloat 1.0,
    --                 Def "Strand" [] [])
    --              , (fromEndpoints 0.1 0.5,
    --                 Def "TranscriptionFactor" ["cobindA", "counbindA"] [])
    --              , (fromEndpoints 0.2 0.7,
    --                 Def "TranscriptionFactor" ["cobindB", "counbindB"] [])
    --              , (fromFloat 0.0,
    --                 Def "Product" [] []) ] :: AbstractProcess Interval)
    it "should parse an expression with an affinity network composed in" $
      parse (process :: Parser (AbstractProcess Double)) "" "[1] S || { e || s at rate MA(1); }"
        `shouldParse`
        mkProcess (AffinityNetworkSpec
                   [Affinity (RateLawAppl "MA" [RateLawParamVal 1.0])
                             [["e"], ["s"]]])
                  [(1.0, Def "S" [] [])]
    it "should parse an expression with an affinity network composed in when surrounded in brackets" $
      parse (process :: Parser (AbstractProcess Double)) "" "([1] S || { e || s at rate MA(1); })"
        `shouldParse`
        mkProcess (AffinityNetworkSpec
                   [Affinity (RateLawAppl "MA" [RateLawParamVal 1.0])
                             [["e"], ["s"]]])
                  [(1.0, Def "S" [] [])]
  describe "model" $ do
    it "should parse the mass action rabbit growth model" $
      parse model "" rabbitSource `shouldParseModel` rabbitModel
  describe "number" $ do
    it "parses floats" $
      parse number "" "2.1" `shouldParse` (2.1 :: Double)
    it "parses integers" $
      parse number "" "2" `shouldParse` (2.0 :: Double)
    it "parses interval basic" $
      parse number "" "[2 .. 2.5]"
      `shouldParse` (fromEndpoints 2 2.5 :: Interval)
    it "parses interval basic with a comma" $
      parse number "" "[2, 2.5]"
      `shouldParse` (fromEndpoints 2 2.5 :: Interval)
    it "parses double as interval" $
      parse number "" "2.5"
      `shouldParse` (fromFloat 2.5 :: Interval)
    it "soundly parsers an inprecicely represenable double as an interval" $
      fmap endpoints (parse number "" "0.333333333333333333333333333333333333333333")
        `parseSatisfies`
        (\(l, u) -> toRational l <= (0.333333333333333333333333333333333333333333 :: Rational)
        && toRational u >= (0.333333333333333333333333333333333333333333 :: Rational))
  describe "symbolic" $ do
    describe "symbIdentifier" $ do
      it "parses lowercase identifiers" $
        parse symbIdentifier "" "k" `shouldParse` "k"
      it "parses uppercase identifiers" $
        parse symbIdentifier "" "L" `shouldParse` "L"
    describe "symbTerm" $ do
      it "parses vars" $
        parse symbTerm "" "x" `shouldParseSym` var "x"
      it "parses decimal vals" $
        parse symbTerm "" "2.1" `shouldParseSym` 2.1
      it "parses integral vals" $
        parse symbTerm "" "2" `shouldParseSym` 2
      it "parses sines" $
        parse symbTerm "" "sin x" `shouldParseSym` sin (var "x")
      it "parses cosines" $
        parse symbTerm "" "cos x" `shouldParseSym` cos (var "x")
      it "parses tangents" $
        parse symbTerm "" "tan x" `shouldParseSym` tan (var "x")
      it "parses exponentials" $
        parse symbTerm "" "exp x" `shouldParseSym` exp (var "x")
      it "parses logarithms" $
        parse symbTerm "" "log x" `shouldParseSym` log (var "x")
      it "parses nested expressions" $
        parse symbTerm "" "sin cos tan x"
          `shouldParseSym` sin (cos (tan (var "x")))
    describe "symbExpr" $ do
      it "parses sums" $
        parse symbExpr "" "sin x + cos y"
          `shouldParseSym` (sin (var "x") + cos (var "y"))
      it "parses products" $
        parse symbExpr "" "2*x*y"
          `shouldParseSym` (2 * var "x" * var "y")
      it "parses nested expessions" $
        parse symbExpr "" "2 * cos (x + y) + sin(z * w)"
          `shouldParseSym` ((2 * cos (var "x" + var "y"))
                       + sin(var "z" * var "w"))
      it "parses hill equation" $
        parse symbExpr "" "L^n / (k + L^n)"
          `shouldParseSym` (var "L"**var "n" / (var "k" + var "L"**var "n"))
