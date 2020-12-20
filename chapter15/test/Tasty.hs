module Main where

import Chapter15.BTEx
import Test.Tasty
import Test.Tasty.HUnit as HU

main :: IO ()
main = defaultMain allTests

hunitTestInsertOnLeaf :: TestTree
hunitTestInsertOnLeaf =
  HU.testCase "Insert 'a' on empty tree" $
  assertEqual "Insertion is wrong" (treeInsert 'a' Leaf) (Node 'a' Leaf Leaf)

allTests :: TestTree
allTests =
  testGroup "Tasty Tests" [testGroup "HUnit Tests" [hunitTestInsertOnLeaf]]
