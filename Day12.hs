module Day12 (main) where

{-# LANGUAGE LambdaCase #-}
import Data.Graph.AStar
import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import Control.Lens
import qualified Data.HashSet as S
import Data.Char
import Debug.Trace
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Data.Ord
import Data.List
import Data.Maybe

findM ::  IntMap (IntMap Char) -> Char -> (Int,Int)
findM x c = x ^@?! folded <.> folded . filtered (==c)
                & fst

main = do
  ijxs <- M.unionsWith M.union . zipWith (\i -> M.singleton i . M.unions . zipWith M.singleton [0 .. ])  [0 .. ] . lines
                <$> readFile "inputs/input12.txt"
  let s = findM ijxs 'S'
  let e = findM ijxs 'E'

  let correctStartEnd = \case
        'S' -> 'a'
        'E' -> 'z'
        x -> x

  let neighs up (i,j) = S.fromList [ (i, j)
                | p <- map correctStartEnd $ ijxs ^.. ix i . ix j,
                  (di,dj) <- [(-1, 0), (1,0), (0,1), (0, -1)],
                  i <- [i+ di],
                  j <- [j+ dj],
                  q <- map correctStartEnd $ ijxs ^.. ix i . ix j,
                  if up then ord q - ord p <= 1
                        else ord p - ord q <= 1
                  ]
  let Just p = aStar (neighs True)
                (\_ _ -> 1)
                (manhatan e)
                (== e)
                s
  print $ length p

  let aPoints =allAs ijxs
      Just p2 = aStar (neighs False)(\_ _ -> 1)
                (closest aPoints)
                (\(i,j) -> not $ nullOf (ix i . ix j) aPoints)
                e
  print $ length p2

manhatan (i,j) (a,b) = abs (i - a) + abs (j - b)

allAs mss = mss ^@.. folded <.> folded . filtered (=='a')
        <&> fst 
        <&> _2 %~ IS.singleton
        & M.fromListWith IS.union

closest m (i,j) = m ^@.. ifolded . to (closestTo j) . _Just
        <&> manhatan (i,j)
        & minimum
  where closestTo k s = lookupClosest k $ catMaybes [IS.lookupLE k s, IS.lookupLE k s]
        lookupClosest a xs = listToMaybe $ map snd $ sortBy (comparing fst) [ (abs (a-x), x) | x <- xs ]

drawP :: IntMap (IntMap Char) -> [(Int,Int)] -> IO ()
drawP mss ijs = putStrLn $ unlines [
                [ case break (==(i,j)) ijs of
                            (_ , _ : (a,b) : _) | a == i -> if b > j then '>' else '<'
                                | j == b -> if a > i then 'v' else '^'
                            _ -> '.'
                  | (j, m) <- M.toList ms ] |
        (i, ms) <- M.toList mss
   ]



