{-# LANGUAGE ScopedTypeVariables #-}

module Chapter_07_Spec where

import Chapter_07
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do

    describe "Exercise 7.1" $ do
        describe "e_7_1" $ do
            it "behaves just like [f x | x <- xs, p x]" $
                property $ \ (Fn(f::Int -> Int)) (Fn(p::Int -> Bool)) (xs::[Int]) ->
                    e_7_1 f p xs == [f x | x <- xs, p x]

    describe "Exercise 7.2" $ do
        describe "myAll" $ do
            it "behaves just like `all`" $
                property $ \ (Fn(p::Char -> Bool)) (xs::[Char]) ->
                    myAll p xs == all p xs 
        describe "myAny" $ do
            it "behaves just like `any`" $
                property $ \ (Fn(p::Char -> Bool)) (xs::[Char]) ->
                    myAny p xs == any p xs 
        describe "myTakeWhile" $ do
            it "behaves just like `takeWhile`" $
                property $ \ (Fn(p::Char -> Bool)) (xs::[Char]) ->
                    myTakeWhile p xs == takeWhile p xs 
        describe "myDropWhile" $ do
            it "behaves just like `dropWhile`" $
                property $ \ (Fn(p::Char -> Bool)) (xs::[Char]) ->
                    myDropWhile p xs == dropWhile p xs 

    describe "Exercise 7.3" $ do
        describe "myMap" $ do
            it "behaves just like `map`" $
                property $ \ (Fn(f::Char -> Int)) (xs::[Char]) ->
                    myMap f xs == map f xs 
        describe "myFilter" $ do
            it "behaves just like `filter`" $
                property $ \ (Fn(p::Char -> Bool)) (xs::[Char]) ->
                    myFilter p xs == filter p xs 
    
    describe "Exercise 7.5" $ do
        describe "myCurry" $ do
            it "behaves just like `curry`" $
                property $ \ (Fn(f::(Char,Char) -> Int)) x y ->
                    myCurry f x y == curry f x y 
        describe "myUncurry" $ do
            it "is the inverse of `curry`" $
                property $ \ (Fn(f::(Char,Char) -> Int)) x y ->
                    myUncurry (curry f) (x,y) == f (x,y) 
