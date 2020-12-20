module Chapter15.BTEx where

-- | A typical binary tree
data BinaryTree a
  = Leaf -- ^Leaves
  | Node a (BinaryTree a) (BinaryTree a) -- ^Inner nodes
  deriving (Show, Eq)

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node y l r)
  | x <= y = Node y (treeInsert x l) r
  | otherwise = Node y l (treeInsert x r)

treeMerge :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeMerge t Leaf = t
treeMerge t (Node x l r) = treeInsert x $ treeMerge (treeMerge t l) r
