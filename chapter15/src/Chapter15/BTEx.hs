-- | Simple implementation of binary trees
module Chapter15.BTEx
    -- * The main data type
  ( BinaryTree(..)
    -- * Operations
    -- ** Insertion
  , treeInsert
  , treeMerge
  , treeFind
  , treeSize
  , treeDelete
  ) where

-- | A typical binary tree
data BinaryTree a
  = Leaf -- ^Leaves
  | Node a (BinaryTree a) (BinaryTree a) -- ^Inner nodes
  deriving (Show, Eq)

{-|
Inserts an element into a 'BinaryTree'

 * If it finds a leaf, insert there

 * If smaller than the item in the node, insert in the left

 * If larger than the item in the node, insert in the right

>>> treeInsert 1 Leaf
Node 1 Leaf Leaf
-}
treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node y l r)
  | x <= y = Node y (treeInsert x l) r
  | otherwise = Node y l (treeInsert x r)

treeMerge :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeMerge t Leaf = t
treeMerge t (Node x l r) = treeInsert x $ treeMerge (treeMerge t l) r

treeFind :: Ord a => a -> BinaryTree a -> Maybe a
treeFind t (Node v l r) =
  case compare t v of
    EQ -> Just v
    LT -> treeFind t l
    GT -> treeFind t r
treeFind _ Leaf = Nothing

{-@ LIQUID "--no-termination" @-}
{-@ treeSize :: BinaryTree a -> Nat @-}
treeSize :: Ord a => BinaryTree a -> Int
treeSize Leaf = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

treeDelete :: Ord a => a -> BinaryTree a -> BinaryTree a
treeDelete _ Leaf = Leaf
treeDelete x (Node y l r)
  | x == y = treeMerge l r
  | x < y = Node y (treeDelete x l) r
  | otherwise = Node y l (treeDelete x r)
