module Chapter_02_Spec where

import Chapter_02
import Test.Hspec
import Test.QuickCheck ()



spec :: Spec
spec = do
    describe "Exercise 2.2" $ do
        it "2^3*4 is correctly parenthesised" $
            e_2_2_a `shouldBe` 2^3*4
        it "2*3+4*5 is correctly parenthesised" $
            e_2_2_b `shouldBe` 2*3+4*5
        it "2+3*4^5 is correctly parenthesised" $
            e_2_2_c `shouldBe` 2+3*4^5


