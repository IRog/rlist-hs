module Main where
import Rlist
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Rlist a) where
  arbitrary = fmap listToRlist (arbitrary @[a])
  shrink rlist = fmap listToRlist $ shrink $ rlistToList rlist

main :: IO ()
main =
  quickCheck prop_list_rlist_tripping
  *> quickCheck prop_cons_uncons

prop_list_rlist_tripping :: [Int] -> Bool
prop_list_rlist_tripping xs = rlistToList (listToRlist xs) == xs

prop_cons_uncons :: Int -> Rlist Int -> Bool
prop_cons_uncons a rlist = case uncons $ cons a rlist of
  Nothing -> False
  Just (item, new_rlist) -> item == a && rlist == new_rlist
