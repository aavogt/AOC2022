module Day17b (main) where



import Control.Lens

import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List
import Data.Maybe (isJust, fromMaybe)
import Control.Monad.Trans.State
import Control.Monad
import Text.Printf

type M = IntMap IntSet
type B = [(Int,Int)]
type S = (B,M, String, Int, Int)


placeAll :: [B] -> State S ()
placeAll (b:bs) = do
  m <- use _2
  _1 .= initialB b m
  let go = do
        ds <- _3 <<%= drop 1
        case ds of
          [] -> return ()
          d:_ -> do
                _4 += 1
                tryBlow d
                hit <- tryDown
                unless hit go
  go
  blowDone <- uses _3 null
  unless blowDone (placeAll bs)
placeAll [] = return ()

tryDown = do
  m <- use _2
  db <- uses _1 down
  let hit = collision db m
  if hit then do
         b <- use _1
         _2 %= landB b
         _5 += 1
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
  -- print mvs
  -- print blocks
  let heightAfter n = evalState ?? ([], M.empty, cycle mvs, 0, 0) $ do
                placeAll (take n (cycle blocks))
                preuse $ _2 . to M.lookupMax . _Just . _1
  -- print (heightAfter 2022)

  let periodTrace mn m = evalState ?? ([], M.empty, take m $ cycle mvs, 0, 0)
                                $ placeTrace mn
      placeTrace mn = do
                placeAll (maybe id take mn $ cycle blocks)
                ht <- preuse $ _2 . to M.lookupMax . _Just . _1
                nJ <- use _4
                nB <- use _5
                return (fromMaybe 0 ht,nJ,nB)
  putStrLn " input for 17.Rmd/hand calculation in comments\nht,nJ,nB"
  sequence_ [ 
      let (a,b,c) = periodTrace Nothing (14700 + k * 10091) 
      in printf "%d,%d,%d\n" a b c :: IO()
       | k <- [0 .. 10] ]
  -- run nj=4609, then afterwards 10091, should end up with round block numbers
  -- looked in R, every
  -- use 1703 blocks to get 2671 use up one repetition of the mvs
  -- subsequent repetitions of mvs only use up 1695
  -- # A tibble: 10 × 3
  --       ht     nJ    nB
  --    <dbl>  <dbl> <dbl>
  --  1  2699  10091  1703
  --  2  5370  20182  3398
  --  3  8041  30273  5093
  --  4 10712  40364  6788
  --  5 13383  50455  8483
  --  6 16054  60546 10178
  --  7 18725  70637 11873
  --  8 21396  80728 13568
  --  9 24067  90819 15263
  -- 10 26738 100910 16958
  -- > diff(d$ht)
  -- [1] 2671 2671 2671 2671 2671 2671 2671 2671 2671
  -- > diff(d$nB)
  -- [1] 1695 1695 1695 1695 1695 1695 1695 1695 1695
  -- > diff(d$nJ)
  -- [1] 10091 10091 10091 10091 10091 10091 10091 10091 10091
  -- > (1000000000000 - 1703) / 1695
  -- [1] 589970500 -- number of repetitions needed
  -- so then the height will be
  --  2699 + 589970500 * 2671
  -- 1575811208199 -- too low
  -- the problem is that 
  -- ghci> (1000000000000 - 1703) `divMod` 1695
  -- (589970500,797)
  -- instead of 1703 as the inital I could place 1703+797 then an integer
  -- number of periods will end with 1000000000000 blocks
  --
  -- The new starting point:
  -- > d
  -- # A tibble: 11 × 3
  --       ht     nJ    nB
  --    <dbl>  <dbl> <dbl>
  --  1  3987  14700  2500
  --  2  6658  24791  4195
  --  3  9329  34882  5890
  --  4 12000  44973  7585
  --  5 14671  55064  9280
  --  6 17342  65155 10975
  --  7 20013  75246 12670
  --  8 22684  85337 14365
  --  9 25355  95428 16060
  -- 10 28026 105519 17755
  -- 11 30697 115610  1945
  -- > diff(d$nB)
  --  [1]   1695   1695   1695   1695   1695   1695   1695   1695   1695 -15810
  -- > diff(d$nJ)
  --  [1] 10091 10091 10091 10091 10091 10091 10091 10091 10091 10091
  -- > diff(d$ht)
  --  [1] 2671 2671 2671 2671 2671 2671 2671 2671 2671 2671
  --
  -- ghci> (1000000000000 - 2500) `divMod` 1695
  -- (589970500,0)
  -- So then the height will be
  -- 589970500*2671 + 3987
  -- 1575811209487
  -- correct!

