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
mapTree f t = case t of
  Leaf a -> Leaf (f a)
  Parent a l r -> Parent (f a) (mapTree f l) (mapTree f r)