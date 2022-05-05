module BinarySearchTree_Spec where

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import qualified BinarySearchTree (T, empty, insert, fromList, toList, member, merge, bubbleSort, restore)


spec :: Spec
spec = do


    describe "BinarySearchTree" $ do
        let defaultTree = BinarySearchTree.fromList [3,98,6,2,8,6,7,5,9]

        describe "fromList" $ do
            it "creates a tree from a even unordered list" $
                BinarySearchTree.fromList [2,3,5,6,6,7,8,9,98] `shouldBe` defaultTree
            it "creates a tree from a odd unordered list" $
                BinarySearchTree.fromList [2,3,5,6,7,8,9,98] `shouldBe` BinarySearchTree.fromList [9,6,3,2,5,98,8,7]
        describe "toList" $ do
            it "creates a ordered list from a tree" $
                BinarySearchTree.toList defaultTree `shouldBe` [2,3,5,6,6,7,8,9,98]

        describe "sort" $ do
            it "sorts an array" $
                BinarySearchTree.bubbleSort [6,16,3,9,2,12,8] `shouldBe` [2,3,6,8,9,12,16]


        describe "inheritance" $ do
            it "determines if a tree is smaller" $
                defaultTree <= BinarySearchTree.fromList [101,150,200] `shouldBe` True
            it "determines if a tree is bigger" $
                defaultTree <= BinarySearchTree.fromList [3,150,200] `shouldBe` False
            it "determines if a tree is equal" $
                defaultTree == BinarySearchTree.insert 2 defaultTree `shouldBe` False
            it "determines if a tree is equal" $
                defaultTree == defaultTree `shouldBe` True
                
            let t1 = BinarySearchTree.fromList [2,1]
            let t2 = BinarySearchTree.fromList [4,5,3]
            let t3 = BinarySearchTree.fromList [7,1,3]
            it "is a member of semigroup" $
                (t1 <> t2) `shouldBe` BinarySearchTree.fromList [2,1,4,5,3]
            it "is a member of monoid with left identity" $
                (t1 <> BinarySearchTree.empty) `shouldBe` t1
            it "is a member of monoid with right identity" $
                (BinarySearchTree.empty <> t1) `shouldBe` t1
            it "is a member of monoid with associativity" $
                (t1 <> (t2 <> t3)) `shouldBe` ((t1 <> t2) <> t3)


        describe "insert" $ do
            it "inserts an element to the right and leaves the order intact" $
                BinarySearchTree.insert 3 (BinarySearchTree.fromList [1,2]) `shouldBe` BinarySearchTree.fromList [1,2,3]
            it "inserts an element to the left and leaves the order intact" $
                BinarySearchTree.insert 1 (BinarySearchTree.fromList [1,2,3]) `shouldBe` BinarySearchTree.fromList [1,2,3,1]

        describe "merge" $ do
            it "merges two trees" $
                BinarySearchTree.merge (BinarySearchTree.fromList [2,1]) (BinarySearchTree.fromList [3]) `shouldBe` BinarySearchTree.fromList [1,2,3]

        describe "member" $ do
            it "determines whether an element is present in a tree" $
                BinarySearchTree.member 2 defaultTree `shouldBe` True
            it "determines whether an element is present in a tree" $
                BinarySearchTree.member 1 defaultTree `shouldBe` False

        describe "restore" $ do
            let unbalanced = BinarySearchTree.insert 3 (BinarySearchTree.insert 98 (BinarySearchTree.insert 6 (BinarySearchTree.insert 2 (BinarySearchTree.fromList [8,6,7,5,9]))))
            it "restores an unbalanced tree to a balanced version of it" $
                BinarySearchTree.restore unbalanced `shouldBe` defaultTree