module Day14 (main) where

{-# LANGUAGE TypeFamilies, BlockArguments #-}
import Data.List.Split
import qualified Data.IntMap as M
import Control.Lens
import qualified Data.IntSet as S
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Control.Exception
import Linear
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad
import Data.Maybe

type M = IntMap IntSet -- blocked paths
type P = [V2 Int]      -- path to the most recent insertion

type S = (P, M, Int, Maybe Int) -- (P, M, initial number, floor height)

insert :: StateT S IO ()
insert = do
  continue <- dlr =<< preuse (_1 . _head)
  when continue insert

finalN :: StateT S IO Int
finalN = do
        n0 <- use _3
        n <- gets (sumOf (_2 . folded . to S.size))
        return (n - n0)

checkBottom i j grow = do
  floor <- use _4
  ledge <- preuse (_2 . ix i . to (S.lookupGT j) . _Just)
  if isJust floor || isJust ledge then grow else return False

dlr :: Maybe (V2 Int) -> StateT S IO Bool
dlr (Just (V2 i j)) = checkBottom i j do
  let blocked a = do
                onFloor <- uses _4 (Just (j+1)==)
                max onFloor . isJust <$> preuse (_2 . ix a . ix (j+1))
      go a = do
        _1 %= (V2 a (j+1) : )
        insert
  b <- blocked i
  if b then do
        l <- blocked (i-1)
        r <- blocked (i+1)
        case (l,r) of
          (False, _) -> go (i-1)
          (True, False) -> go (i+1)
          _ -> do
            _1 %= drop 1
            _2 . at i . non mempty . at j ?= ()
   else go i
  return True
dlr Nothing = return False

segsToM :: [[[Int]]] -> M
segsToM = M.unionsWith S.union . map segToM

segToM :: [[Int]] -> M
segToM ([x1,y1] : [x2,y2] : xs) = M.unionWith S.union segAB (segToM ([x2,y2] : xs))
 where
   segAB = M.unionsWith S.union $ if
        x1 == x2 then [ M.singleton x1 (S.singleton y)  | y <- [min y1 y2 .. max y1 y2 ] ]
                 else assert (y1 == y2)
                      [ M.singleton x (S.singleton y1)  | x <- [min x1 x2 .. max x1 x2 ] ]
segToM _ = M.empty

floorHeight :: M -> Int
floorHeight = (2+) . fromJust . maximumOf (folded . to S.maxView . _Just . _1)

main = do
  -- uu-parsinglib would be better
  segs <- segsToM . map (map (map (\x -> read x :: Int) . splitOn ",") . splitOn " -> ") . lines <$> readFile "inputs/input14.txt"
  let s0 = ([V2 500 0], segs, sumOf (folded . to S.size) segs, Nothing)
      sol = do
        insert
        lift . print =<< finalN

  -- part 1
  runStateT sol s0

  -- part 2
  runStateT ?? s0 $ do
        _4 ?= floorHeight segs
        sol

  return ()
