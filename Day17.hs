module Day17 (main) where



import Control.Lens

import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List
import Data.Maybe (isJust, fromMaybe)
import Control.Monad.Trans.State
import Control.Monad

type M = IntMap IntSet
type B = [(Int,Int)]
type S = (B,M, String, Int, Int)


placeAll :: [B] -> State S ()
placeAll (b:bs) = do
  m <- use _2
  _1 .= initialB b m
  _5 += 1
  let go = do
        ~(d:_) <- _3 <<%= tail
        _4 += 1
        tryBlow d
        hit <- tryDown
        unless hit go
  go
  placeAll bs
placeAll [] = return ()

tryDown = do
  m <- use _2
  db <- uses _1 down
  let hit = collision db m
  if hit then do
         b <- use _1
         _2 %= landB b
    else _1 .= db
  return hit

landB :: B -> M -> M
landB = M.unionWith S.union . f
 where f :: B -> M
       f = M.fromListWith S.union . (mapped . _2 %~ S.singleton)


tryBlow d = do
  m <- use _2
  _1 %= \b ->
    let bb = blow d b
    in if collision bb m then b else bb


blocks :: [B]
blocks = map (concat . reverse . zipWith (\i s -> map (i,) $ elemIndices '@' s) [0 :: Int .. ] . 
               reverse . words . filter (/='|')) [
  "|..@@@@.|",
  "|...@...| |..@@@..| |...@...|",
  "|....@..| |....@..| |..@@@..|",
  "|..@....| |..@....| |..@....| |..@....|",
  "|..@@...| |..@@...|"]

collision :: B -> M -> Bool
collision b m = any f b
 where
  f :: (Int,Int) -> Bool
  f (0, n) = True
  f (i, n) | n < 0 || n > 6 = True
  f (i, j) = isJust $ m ^? ix i . ix j

blow :: Char -> B -> B
blow c xs = xs <&> _2 +~ case c of
        '>' -> 1
        '<' -> -1

down :: B -> B
down = mapped . _1 -~ 1

initialB :: B -> M -> B
initialB b0 m =
   let h = maybe 0 fst $ M.lookupMax m
   in b0 <&> _1 +~ (h+4)

main = do
  mvs <- concat . lines <$> readFile "inputs/input17.txt"
  let heightAfter n = evalState ?? ([], M.empty, cycle mvs, 0, 0) $ do
                placeAll (take n (cycle blocks))
                preuse $ _2 . to M.lookupMax . _Just . _1
  print (heightAfter 2022)
