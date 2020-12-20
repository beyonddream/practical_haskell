module Chapter15.BTEx where

data BinaryTree a
  = Leaf
  | Node a (BinaryTree a) (BinaryTree a)
  deriving (Show, Eq)

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node y l r)
  | x <= y = Node y (treeInsert x l) r
  | otherwise = Node y l (treeInsert x r)

treeMerge :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeMerge t Leaf = t
treeMerge t (Node x l r) = treeInsert x $ treeMerge (treeMerge t l) r
