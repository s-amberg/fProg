{-# LANGUAGE ScopedTypeVariables #-}

module Chapter_01_Spec where

import Chapter_01
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "Exercise 1.1" $ do
        describe "myProduct" $ do
            it "myProduct [2,3,4] == 24" $
                myProduct [2,3,4] `shouldBe` 24
            it "behaves like `product`" $
                property $ \(xs::[Int]) -> 
                    myProduct xs == product xs




