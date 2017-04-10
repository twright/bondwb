module CPi.ParserNewSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Test.QuickCheck
import CPi.AST
import CPi.ParserNew
import Data.Hashable (hash)

a `shouldParseNf` b = a `parseSatisfies` (\p -> normalForm p == normalForm b)

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
    it "should be the left inverse of pretty printing, upto normal form" $
      property (\x -> let converted = normalForm $ relocateAll flocs hlocs x
                          flocs     = freeLocs x
                          hlocs     = map (fromIntegral . hash . show) flocs
                      in (fmap normalForm $ parse species "" $ pretty x)
                         === Right converted)
