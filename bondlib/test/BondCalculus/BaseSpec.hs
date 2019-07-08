module BondCalculus.BaseSpec (spec) where

import Test.Hspec
import BondCalculus.Base
import Test.QuickCheck

spec :: SpecWith ()
spec = do
    describe "Interval" $ do
        describe "show" $
            it "shows a basic interval" $
                show (fromEndpoints 1.0 2.0)
                `shouldBe`
                "[1.000000 .. 2.000000]"
        describe "endpoints" $
            it "gives the endpoints of an interval" $
                endpoints (fromEndpoints 1.0 2.0)
                `shouldBe`
                (1.0, 2.0)