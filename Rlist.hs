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

uncons :: Rlist a -> Maybe (a, Rlist a)
uncons (MakeRlist []) = Nothing
uncons (MakeRlist (MakeNode sz (Leaf item) : rest))
  | sz == 1 = Just (item, MakeRlist rest)
  | otherwise = error "uncons: leaf size invariant violated"
uncons (MakeRlist (MakeNode sz (Parent item l r) : rest)) =
  let new_size = div (sz - 1) 2
      new_list = MakeRlist $ MakeNode new_size l : MakeNode new_size r : rest
  in Just (item, new_list)

hd :: Rlist a -> Maybe a
hd (MakeRlist []) = Nothing
hd (MakeRlist (MakeNode sz (Leaf item) : _tl))
  | sz == 1 = Just item
  | otherwise = error "hd: leaf size invariant violated"
hd (MakeRlist (MakeNode _size (Parent item _l _r) : _tl)) = Just item

tl :: Rlist a -> Rlist a
tl (MakeRlist []) = empty
tl (MakeRlist (MakeNode sz (Leaf _item) : rest))
  | sz == 1 = MakeRlist rest
  | otherwise = error "tl: leaf size invariant violated"
tl (MakeRlist (MakeNode _size (Parent _item _l _r) : rest)) = MakeRlist rest

empty :: Rlist a
empty = MakeRlist []

size :: Rlist a -> Int
size = go 0 where
  go acc (MakeRlist []) = acc
  go acc (MakeRlist ((MakeNode sz _tree) : rest)) =
    go (acc + sz) (MakeRlist rest)

indexTree :: Int -> Int -> Tree a -> a
indexTree indx _size (Leaf item)
  | indx == 0 = item
  | otherwise = error "indexTree: leaf size invariant violated"
indexTree indx sz (Parent item left right)
  | indx == 0 = item
  | otherwise =
    let subtree_size = div (sz - 1) 2
    in if indx <= subtree_size
       then indexTree (indx - 1) subtree_size left
       else indexTree (indx - subtree_size - 1) subtree_size right

index :: Int -> Rlist a -> Maybe a
index _idx (MakeRlist []) = Nothing
index idx (MakeRlist ((MakeNode sz tree) : rest))
  | idx < sz = Just $ indexTree idx sz tree
  | otherwise = index (idx - sz) (MakeRlist rest)
