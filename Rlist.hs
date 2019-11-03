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