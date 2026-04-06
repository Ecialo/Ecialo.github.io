module MoldSpec where

import Test.Hspec

spec :: Spec
spec = do
    describe "Mold" $ do
        it "computes volume for RoundMold correctly" $ do
            pending