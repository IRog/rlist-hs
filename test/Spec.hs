module Main where
import Rlist.Internal
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Rlist a) where
  arbitrary = fmap listToRlist (arbitrary @[a])
  shrink rlist = fmap listToRlist $ shrink $ rlistToList rlist

main :: IO ()
main =
  quickCheck prop_list_rlist_tripping
  *> quickCheck prop_cons_uncons
  *> quickCheck prop_map_identity
  *> quickCheck prop_size_list_oracle
  *> quickCheck prop_max_mesenne_naive
  *> quickCheck prop_list_to_rlistN

prop_list_rlist_tripping :: [Int] -> Bool
prop_list_rlist_tripping xs = rlistToList (listToRlist xs) == xs

prop_cons_uncons :: Int -> Rlist Int -> Bool
prop_cons_uncons a rlist = case uncons $ cons a rlist of
  Nothing -> False
  Just (item, new_rlist) -> item == a && rlist == new_rlist

prop_map_identity :: Rlist Int -> Bool
prop_map_identity rlist = fmap id rlist == rlist

prop_size_list_oracle :: Rlist Int -> Bool
prop_size_list_oracle xs = size xs == length (rlistToList xs)

prop_max_mesenne_naive :: Positive Int -> Bool
prop_max_mesenne_naive x =
  let it = getPositive x
  in maxMersenneUpToNaive it == maxMersenneUpTo it

prop_list_to_rlistN :: [Int] -> Bool
prop_list_to_rlistN xs =
  listToRlistN (length xs) xs == listToRlist xs

isBalanced :: Tree a -> Bool
isBalanced = snd . isBalancedWorker

isBalancedWorker :: Tree a -> (Int, Bool)
isBalancedWorker Leaf{} = (1, True)
isBalancedWorker (Parent _ l r) =
  let (l_size, lbl) = isBalancedWorker l
      (r_size, rbl) = isBalancedWorker r
  in (l_size + 1, l_size == r_size && lbl && rbl)

