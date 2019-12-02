module Main where

import Rlist.Internal

import Control.DeepSeq(rnf)
import Control.Exception(evaluate)
import Criterion.Main
import Criterion.Types

main :: IO ()
main = do
  let lil = [1] :: [Int]
  let big = [1..1000] :: [Int]
  let bigger = [1..30000] :: [Int]
  let biggest = [1..100000] :: [Int]
  evaluate (rnf lil)
  evaluate (rnf big)
  evaluate (rnf bigger)
  evaluate (rnf biggest)
  defaultMainWith config
    [ bgroup "rlist"
      [ bgroup "small"
        [ bench "listToRlistN" $
            nf (listToRlistN 1) lil
        , bench "listToRlistN'" $
            nf (listToRlistN' 1) lil
        , bench "listToRlist" $ nf listToRlist lil
        ]
      ]
    , bgroup "big"
      [ bench "listToRlistNBig" $
          nf (listToRlistN 1000) big
      , bench "listToRlistN'Big" $
          nf (listToRlistN' 1000) big
      , bench "listToRlistBig" $ nf listToRlist big
      ]
    , bgroup "bigger"
      [ bench "listToRlistNBigger" $
          nf (listToRlistN 30000) bigger
      , bench "listToRlistN'Bigger" $
          nf (listToRlistN' 30000) bigger
      , bench "listToRlistBigger" $ nf listToRlist bigger
      ]
    , bgroup "biggest"
      [ bench "listToRlistNBiggest" $
          nf (listToRlistN 100000) biggest
      , bench "listToRlistN'Biggest" $
          nf (listToRlistN' 100000) biggest
      , bench "listToRlistBiggest" $ nf listToRlist biggest
      ]
    ]
  where
    config = defaultConfig {reportFile = Just "rlist.html"}