{-# LANGUAGE ScopedTypeVariables #-}

module Chapter_05_Spec where

import Chapter_05
import Test.Hspec
import Test.QuickCheck
import Data.List ( elemIndices )

spec :: Spec
spec = do
    describe "Exercise 5.1" $ do
        it "Computes the sum of squares of the first 100 integers" $
            e5_1 `shouldBe` 338350
    describe "Exercise 5.4" $ do
        it "`myReplicate` replicates a given character 5 times" $
            myReplicate 5 'a' `shouldBe` "aaaaa"
        it "`myReplicate` behaves just like its reference implementation `replicate`" $
            property $ \(n::Int) (xs::[Int]) -> 
                myReplicate n xs == replicate n xs
    describe "Exercise 5.8" $ do
        it "`position` correctly finds all positions of o in potato " $
            positions 'o' "potato" `shouldBe` [1,5]
        it "`positions` behaves just like its reference implementation `elemIndices`" $
            property $ \(x::Char) (xs::[Char]) -> 
                positions x xs == elemIndices x xs

