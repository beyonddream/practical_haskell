module Main where

import Chapter15.BTEx
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit as HU

main :: IO ()
main = defaultMain allTests

hunitTestInsertOnLeaf :: TestTree
hunitTestInsertOnLeaf =
  HU.testCase "Insert 'a' on empty tree" $
  assertEqual "Insertion is wrong" (treeInsert 'a' Leaf) (Node 'a' Leaf Leaf)

hunitTestSizeOfTree :: TestTree
hunitTestSizeOfTree =
  HU.testCase "Size of a tree" $
  assertEqual
    "Size is wrong"
    (treeSize (treeInsert 'a' (treeInsert 'b' Leaf)))
    2
        {- HLINT ignore hunitTestDeleteOnTree -}

hunitTestDeleteOnTree :: TestTree
hunitTestDeleteOnTree =
  HU.testCase "Cannot find deleted element in a tree" $
  (isNothing $ treeFind 'a' $ treeDelete 'a' $ treeInsert 'a' Leaf) HU.@?
  "Can find element"

allTests :: TestTree
allTests =
  testGroup
    "Tasty Tests"
    [ testGroup
        "HUnit Tests"
        [hunitTestInsertOnLeaf, hunitTestSizeOfTree, hunitTestDeleteOnTree]
    ]
