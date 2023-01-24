{-# LANGUAGE BlockArguments #-}
module Day09 (main) where

import Linear
import Control.Monad.Trans.State
import Control.Monad
import Control.Lens
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.List
import Control.Monad.Trans.Class
import Data.Foldable

type P = V2 Int
type S = (P,P, [P], IntMap IntSet)

-- mvs (d, c) = replicateM_ c (mv d)

-- I could reuse mv but now each tail can be a head
-- So I just need to store the path
mvHeadCommand :: Char -> StateT S IO ()
mvHeadCommand c = do
   case c of
     'R' -> _1 . _y += 1
     'L' -> _1 . _y -= 1
     'U' -> _1 . _x -= 1
     'D' -> _1 . _x += 1

tailFollow :: StateT S IO ()
tailFollow = do
   h <- use _1
   _2 %= \t -> t + case qd h t of
                                0 -> 0 -- same square
                                1 -> 0 -- one coord differs by 1
                                2 -> 0 -- two coords differ by one
                                _ -> pmin 1 (h-t)


tailRecordT :: StateT S IO ()
tailRecordT = do
   t <- use _2
   _3 %= (t : ) -- missing one

pmin (V2 a b) (V2 c d) = V2 (min0 a c) (min0 b d)
 where min0 a b = signum b * min (abs a) (abs b) 

tailRecord :: StateT S IO ()
tailRecord = do
   V2 tx ty <- use _2
   _4 . at tx . non mempty %= S.insert ty


mvsN :: Int -> [(Char, Int)] -> StateT S IO Int
mvsN nsegments cs = do
  for_ cs \ (c,n) -> replicateM n do
        mvHeadCommand c
        h <- use _1
        _3 %= (h : )
  hps <- replicateM nsegments do
    newHP <- uses _3 reverse
    put (0,0, [], M.singleton 0 (S.singleton 0))
    for_ newHP \h -> do
      _1 .= h
      tailFollow
      tailRecordT
      tailRecord -- only needed with the last iteration
    return newHP
  -- lift $ mapM (ppHPS 26) (Data.List.transpose hps)
  uses _4 $ sumOf (folded . to S.size)

ppHPS :: Int -> [P] -> IO ()
ppHPS b s = unless (all (=='.') $ concat dots) $ putStrLn (unlines dots)
 where dots = [ [ case elemIndex p s of
                                Just n -> head (show n)
                                _ -> '.' 
                | j <- [0 .. b-1],
                  let p = V2 i j ]
                | i <- [0 .. b-1] ]


pp :: Int -> S -> IO ()
pp b s = unless (all (=='.') $ concat dots) $ putStrLn (unlines dots)
 where dots = [ [ case elemIndex p [s^._1, s^._2] of
                                Just 0 -> 'H'
                                Just 1 -> 'T'
                                _ -> '.' 
                | j <- [0 .. b-1],
                  let p = V2 i j ]
                | i <- [0 .. b-1] ]

main = do
  steps <- map ((\[[d],n] -> (d, read n :: Int)) . words) . lines <$> readFile "inputs/input9.txt"
  print =<< mvsN 1 steps `evalStateT` (0,0, [], M.singleton 0 (S.singleton 0))
  print =<< mvsN 9 steps `evalStateT` (0,0, [], M.singleton 0 (S.singleton 0))
