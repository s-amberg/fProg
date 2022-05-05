{-# LANGUAGE ScopedTypeVariables #-}

module Chapter_06_Spec where

import Chapter_06
import Test.Hspec
import Test.QuickCheck
import Data.List


mergeRef :: Ord a => [a] -> [a] -> [a]
mergeRef [] ys = ys
mergeRef xs [] = xs
mergeRef (x:xs) (y:ys) =   if x <= y then
                            x : mergeRef xs (y:ys)
                        else
                            y : mergeRef (x:xs) ys

spec :: Spec
spec = do

    describe "Exercise 6.1" $ do
        describe "sumdown" $ do
            it "works for 3" $
                sumdown 3 `shouldBe` 6
            it "works for 0" $
                sumdown 0 `shouldBe` 0 
            it "behaves just like `sum [0..n]` for non-negative integers" $
                property $ \ (n::Int) -> 
                    sumdown (abs n) == sum [1..(abs n)]

    describe "Exercise 6.3" $ do
        describe "(^#)" $ do
            it "behaves just like (^) for non-negative integers" $
                property $ \(n::Int) (m::Int) ->
                    abs n ^# abs m == abs n ^ abs m

    describe "Exercise 6.6" $ do
        describe "myAnd" $ do
            it "behaves just like `and`" $
                property $ \(bs::[Bool]) ->
                    myAnd bs == and bs
        describe "myConcat" $ do
            it "behaves just like `concat`" $
                property $ \(xss::[[Char]]) ->
                    myConcat xss == concat xss
        describe "myReplicate" $ do
            it "behaves just like `replicate`" $
                property $ \(n::Int) (xs::[Int]) -> 
                    myReplicate (abs n) xs == replicate (abs n) xs
        describe "(!!#)" $ do
            it "behaves just like (!!)" $
                property $ \(x::Char) (xs::[Char]) (n::Int) -> let i = (abs n `mod` length (x:xs)) in
                        (x:xs) !!# i == (x:xs) !! i 
        describe "myElem" $ do
            it "behaves just like elem" $
                property $ \(x::Char) (xs::[Char]) ->
                        (x `myElem` xs) == (x `elem` xs)
        
    describe "Exercise 6.7" $ do
        describe "merge" $ do
            it "merges lists correctly" $
                 property $ \(xs::SortedList Int) (ys::SortedList Int)->
                    merge (getSorted xs) (getSorted ys) == mergeRef (getSorted xs) (getSorted ys)

    describe "Exercise 6.8" $ do
        describe "msort" $ do
            it "sorts lists just like Data.List.sort" $
                property $ \(xs::[Int]) ->
                    msort xs == sort xs
