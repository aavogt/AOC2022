module Day06 (main) where

import Control.Lens
import Data.List
import qualified Data.Sequence as S
import qualified Data.IntMap as M
import Data.Foldable
import Data.Char
import Debug.Trace
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8

nubIsEq :: L8.ByteString -> Bool
nubIsEq xs = all (== 1) [ B8.count c (L8.toStrict xs) | c <- L8.unpack xs ]

firstEq n xs = L8.tails xs ^@? itraversed . filtered (nubIsEq . L8.take n)
        <&> (fromIntegral n+) . fst

main = do
  let f = "inputs/input6.txt"
  -- 100s to do these:
  -- print . solve 4 =<< readFile f
  -- print . solve 14 =<< readFile f
  -- 30 s to do these:
  print . firstEq 4 =<< L8.readFile f
  print . firstEq 14 =<< L8.readFile f
