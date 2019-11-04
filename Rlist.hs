module Rlist where
data Tree a
  = Leaf a
  | Parent a (Tree a) (Tree a)
  deriving (Show)

data Node a
  = MakeNode Int (Tree a)
  deriving (Show)

data Rlist a
  = MakeRlist [Node a]
  deriving (Show)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a)       = Leaf (f a)
mapTree f (Parent a l r) =
  Parent (f a) (mapTree f l) (mapTree f r)

mapNode :: (a -> b) -> Node a -> Node b
mapNode f (MakeNode i t) = MakeNode i (mapTree f t)

mapRlist :: (a -> b) -> Rlist a -> Rlist b
mapRlist f (MakeRlist ns) = MakeRlist (map (mapNode f) ns)

cons :: a -> Rlist a -> Rlist a
cons item (MakeRlist (MakeNode n t0 : MakeNode m t1 : rest))
  | n == m = MakeRlist $ MakeNode (n * 2 + 1) (Parent item t0 t1) : rest
cons item (MakeRlist rlist) = MakeRlist $ MakeNode 1 (Leaf item) : rlist

empty :: Rlist a
empty = MakeRlist []

indexTree :: Int -> Int -> Tree a -> a
indexTree index size (Leaf item)
  | index == 0 = item
indexTree index size (Parent item left right)
  | index == 0 = item
  | otherwise =
    let subtree_size = div (size - 1) 2
    in if index <= subtree_size
       then indexTree (index - 1) subtree_size left
       else indexTree (index - subtree_size - 1) subtree_size right
