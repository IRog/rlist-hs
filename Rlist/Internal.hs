module Rlist.Internal
where
import GHC.Exts(IsList(..))
import Data.Foldable(foldl')
import qualified Data.Bits as Bits
import Control.Exception(assert)

data Tree a
  = Leaf a
  | Parent a (Tree a) (Tree a)
  deriving (Eq, Show)

data Node a
  = MakeNode {-# unpack #-} !Int (Tree a)
  deriving (Eq, Show)

newtype Rlist a
  = MakeRlist [Node a]
  deriving (Eq, Show)

isMersenne :: Int -> Bool
isMersenne 0  = False
isMersenne !n = assert (n > 0) $ Bits.popCount (n + 1) == 1

nextMersenne :: Int -> Int
nextMersenne n = assert (isMersenne n) $ 2 * n + 1

maxMersenneUpTo :: Int -> Int
maxMersenneUpTo n
  | isMersenne n = n
  | otherwise = assert (n > 0) $
    let lz = Bits.countLeadingZeros n
    in Bits.shiftL 1 (63 - lz) - 1

maxMersenneUpToNaive :: Int -> Int
maxMersenneUpToNaive n = assert (n > 0) $
  last $ takeWhile (<=n) $ filter isMersenne [0..]

decompose :: Int -> [Int]
decompose = go [] where
  go acc 0  = acc
  go acc !n = assert (n > 0) $
    let !new_mersenne = maxMersenneUpTo n
        !new_n        = n - new_mersenne
    in go (new_mersenne : acc) new_n

instance IsList (Rlist a) where
  type Item (Rlist a) = a
  fromList xs = listToRlistN (length xs) xs
  fromListN = listToRlistN
  toList = rlistToList

instance Semigroup (Rlist a) where
  (<>) a b = listToRlist $ rlistToList a <> rlistToList b

instance Monoid (Rlist a) where
  mempty = empty

instance Functor Tree where
  fmap = mapTree

instance Functor Node where
  fmap = mapNode

instance Functor Rlist where
  fmap = mapRlist

instance Foldable Tree where
  foldr = lazyReduceTree
  foldl' = reduceTree

instance Foldable Rlist where
  foldr = lazyReduce
  foldl' = reduce
  length = size
  null = isEmpty

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
  in seq new_size $ Just (item, new_list)

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

isEmpty :: Rlist a -> Bool
isEmpty (MakeRlist []) = True
isEmpty _any = False

size :: Rlist a -> Int
size = go 0 where
  go acc (MakeRlist []) = acc
  go acc (MakeRlist ((MakeNode sz _tree) : rest)) =
    let new_acc = acc + sz
    in seq new_acc $ go new_acc (MakeRlist rest)

indexTree :: Int -> Int -> Tree a -> a
indexTree !indx _size (Leaf item)
  | indx == 0 = item
  | otherwise = error "indexTree: leaf size invariant violated"
indexTree !indx !sz (Parent item left right)
  | indx == 0 = item
  | otherwise =
    let !subtree_size = div (sz - 1) 2
    in if indx <= subtree_size
       then indexTree (indx - 1) subtree_size left
       else indexTree (indx - subtree_size - 1) subtree_size right

index :: Int -> Rlist a -> Maybe a
index _idx (MakeRlist []) = Nothing
index idx (MakeRlist ((MakeNode sz tree) : rest))
  | idx < sz = Just $ indexTree idx sz tree
  | otherwise = index (idx - sz) (MakeRlist rest)

modifyTree :: (a -> a) -> Int -> Int -> Tree a -> Tree a
modifyTree fn 0 1 (Leaf item) = Leaf (fn item)
modifyTree _fn _idx _sz (Leaf _item) =
  error "modifyTree: leaf size invariant violated"
modifyTree fn !idx !sz (Parent item l r)
  | idx == 0 = Parent (fn item) l r
  | otherwise =
    let !subtree_size = div (sz - 1) 2
    in if idx <= subtree_size
       then Parent item (modifyTree fn (idx - 1) subtree_size l) r
       else Parent item l (modifyTree fn (idx - 1 - subtree_size) subtree_size r)

modify :: (a -> a) -> Int -> Rlist a -> Maybe (Rlist a)
modify _fn _idx (MakeRlist []) = Nothing
modify fn !idx (MakeRlist (node@(MakeNode sz tree) : rest))
  | idx < sz = Just $ MakeRlist $ MakeNode sz (modifyTree fn idx sz tree) : rest
  | otherwise = case modify fn (idx - sz) (MakeRlist rest) of
    Nothing -> Nothing
    Just (MakeRlist rlist) -> Just $ MakeRlist (node : rlist)

reduceTree :: (b -> a -> b) -> b -> Tree a -> b
reduceTree fn !acc (Leaf item) = fn acc item
reduceTree fn !acc (Parent item l r) =
  let !new_acc = fn acc item
      !newer_acc = reduceTree fn new_acc l
  in reduceTree fn newer_acc r

reduce :: (b -> a -> b) -> b -> Rlist a -> b
reduce fn !acc (MakeRlist nodes) =
  foldl' (\ !new_acc (MakeNode _sz tree) -> reduceTree fn new_acc tree) acc nodes

lazyReduceTree :: (a -> b -> b) -> b -> Tree a -> b
lazyReduceTree fn acc (Leaf item) = fn item acc
lazyReduceTree fn acc (Parent item l r) =
  fn item $ lazyReduceTree fn (lazyReduceTree fn acc r) l

lazyReduce :: (a -> b -> b) -> b -> Rlist a -> b
lazyReduce fn acc (MakeRlist nodes) =
  foldr (\ (MakeNode _sz tree) new_acc -> lazyReduceTree fn new_acc tree) acc nodes

listToRlistN :: forall a. Int -> [a] -> Rlist a
listToRlistN n xs = MakeRlist (go (decompose n) xs) where
  go :: [Int] -> [a] -> [Node a]
  go [      ] [] = []
  go [      ] _  = error $ "listToRlistN: " <> show n
  go (m : ms) ys =
    let (node, rest) = mkNode m ys
    in node : go ms rest

mkNode :: forall a. Int -> [a] -> (Node a, [a])
mkNode sz lst =
  assert (isMersenne sz) $
  let (res, rems) = go sz lst
  in (MakeNode sz res, rems)
  where
    go :: Int -> [a] -> (Tree a, [a])
    go _  [      ] = error $ "mkNode invalid tree: " <> show sz
    go 1  (x : xs) = (Leaf x, xs)
    go !n (x : xs) =
      let !i      = div n 2
          (l, ys) = go i xs
          (r, zs) = go i ys
      in (Parent x l r, zs)

rlistToList' :: Rlist a -> [a]
rlistToList' rlist = reverse $ reduce (\ xs x -> x : xs) [] rlist

rlistToList :: Rlist a -> [a]
rlistToList rlist = lazyReduce (:) [] rlist

listToRlist :: [a] -> Rlist a
listToRlist xs = foldr cons empty xs
