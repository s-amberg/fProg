{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Chapter_12_Spec where

import Prelude hiding (Maybe, Nothing, Just)
import Chapter_12
import Test.Hspec
import Test.QuickCheck
import Test.Validity


instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary :: Gen (Maybe a)
  arbitrary = oneof [return Nothing, (pure Just) <*> arbitrary]

-- >>> sample' (arbitrary :: Gen (Maybe Int))
-- [Just 0,Just 0,Just (-4),Just (-1),Nothing,Just 1,Nothing,Nothing,Nothing,Nothing,Just 14]

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary :: Gen (Tree a)
    arbitrary = sized tree
      where
        tree 0 =  pure Leaf
        tree n | n > 0 = pure Node <*> (subtree n) <*> arbitrary <*> (subtree n)
        subtree n = oneof [pure Leaf, tree (n `div` 2)]

-- >>> sample' (arbitrary :: Gen (Tree Int))
-- [Leaf,Node (Node Leaf (-1) Leaf) 1 (Node Leaf 2 Leaf),Node Leaf (-1) Leaf,Node Leaf (-6) (Node (Node Leaf (-5) Leaf) (-4) (Node Leaf (-4) Leaf)),Node Leaf 2 (Node Leaf 1 (Node (Node Leaf (-1) Leaf) (-4) Leaf)),Node Leaf (-8) (Node (Node (Node Leaf (-1) Leaf) (-7) (Node Leaf 2 Leaf)) 10 Leaf),Node Leaf 8 Leaf,Node Leaf (-13) (Node Leaf 14 Leaf),Node Leaf 2 Leaf,Node Leaf (-12) (Node (Node Leaf (-14) Leaf) 16 Leaf),Node (Node (Node Leaf (-9) Leaf) (-5) (Node Leaf (-20) Leaf)) 13 (Node (Node (Node Leaf (-14) Leaf) 4 Leaf) 4 Leaf)]


spec :: Spec
spec = do

    describe "Exercise 12.WarmUp1" $ do
        describe "Maybe" $ do
            functorSpecOnArbitrary @Maybe
            applicativeSpecOnArbitrary @Maybe
            monadSpecOnArbitrary @Maybe
    
    describe "Exercise 12.1" $ do
        describe "Tree" $ do
            functorSpecOnArbitrary @Tree
