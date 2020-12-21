{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Chapter15.BTEx
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC

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

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

allTests :: TestTree
allTests =
  testGroup
    "Tasty Tests"
    [ testGroup
        "HUnit Tests"
        [hunitTestInsertOnLeaf, hunitTestSizeOfTree, hunitTestDeleteOnTree]
    , testGroup
        "Tests over reverse"
        [ QC.testProperty "reverse respects length" $ \(lst :: [Integer]) ->
            length (reverse' lst) == length lst
        , QC.testProperty "applying twice return original list" $ \(lst :: [Integer]) ->
            reverse' (reverse' lst) == lst
        , QC.testProperty "head is last ele of reversed list" $ \(lst :: [Integer]) ->
            if length lst > 0
              then head (reverse' lst) == last lst
              else True
        ]
    ]
