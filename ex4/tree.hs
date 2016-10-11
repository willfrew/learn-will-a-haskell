data BinaryTree a =
    Node (BinaryTree a) a (BinaryTree a)
  | Leaf a
  | NullNode
  deriving Show

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x NullNode = Leaf x
insert x (Leaf y)
  | x <= y    = Node (Leaf x) y NullNode
  | otherwise = Node (Leaf y) x NullNode
insert x (Node l y r)
  | x <= y    = Node (insert x l) y r
  | otherwise = Node l y (insert x r)
