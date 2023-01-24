module Day18 (main) where

import Data.BoundingBox
import Linear
import Control.Lens
import qualified Data.IntSet.Lens as S
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List.Split
import Data.Maybe

type M = IntMap (IntMap IntSet)

ixs :: M -> [V3 Int]
ixs m = m ^@.. ifolded <.> ifolded <. S.members
        <&> \ ((i,j),k) -> V3 i j k

sxi :: [V3 Int] -> M
sxi ijs= M.unionsWith (M.unionWith S.union)
  [ M.singleton i $ M.singleton j $ S.singleton k |
    V3 i j k <- ijs]

neighs :: V3 Int -> [V3 Int]
neighs x = [ x & le +~ pm | pm <- [ -1 ,1], le <- [_1,_2,_3] ]

solve :: [V3 Int] -> (V3 Int -> Bool) -> Int
solve xs isCovered = 6*length xs - length covered
 where covered = [ () | x <- xs, ijk <- neighs x, isCovered ijk ]

main = do
  inp <- readFile "inputs/input18.txt" <&> map ((\[a,b,c] -> V3 a b c) . map read . splitOn ",") . lines
  let inpM = sxi inp
  print $ solve inp (`mmember` inpM)

  let bb = inflate 1 $ foldl1 union [ Box i i | i <- inp ]
  let externalNeighbours x = neighs x
                & filter (`isInside` bb)
                & filter (\ijk -> not $ mmember ijk inpM)

      -- flood fill from one corner of the expanded-by-1 bounding box to the other
      -- excluding points inside inp
      external = iterate (\(g,d) -> -- (growing,dead)
                        let d' = merge g d
                        in (getNS externalNeighbours g \\ d', d'))
                    (sxi [boxFst bb], mempty)
                & takeWhile ((boxSnd bb `notMMember`) . fst)
                & last
                & snd

  print $ solve inp (\ x -> mmember x inpM || notMMember x external)

(\\) :: M -> M -> M
(\\) = M.differenceWith ((Just .) . M.differenceWith ((Just .) .  S.difference))

merge :: M -> M -> M
merge = M.unionWith (M.unionWith S.union)

mmember :: V3 Int -> M -> Bool
mmember (V3 i j k) m = isJust $ m ^? ix i . ix j . ix k

notMMember :: V3 Int -> M -> Bool
notMMember a b = not (mmember a b)

getNS :: (V3 Int -> [V3 Int]) -> M -> M
getNS n m = sxi $ ixs m >>= n

boxFst (Box a _) = a
boxSnd (Box _ a) = a
