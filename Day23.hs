module Day23 (main) where


import Control.Lens
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import qualified Data.IntSet as S
import qualified Data.IntSet.Lens as S
import qualified Data.IntMap as M
import Data.Maybe
import qualified Data.Map as Map
import Data.BoundingBox
import Linear
import Control.Monad

type M = IntMap IntSet

toM :: [String] -> M
toM xss = M.unionsWith S.union
 [ M.singleton i (S.singleton j) | (i, xs) <- zip [0 .. ] xss,
                (j, '#') <- zip [0 .. ] xs]

step m stepN = (m :: M) ^@.. ifolded <. S.members
        & mapMaybe (\x -> flip Map.singleton (Just x) <$> propose1 stepN m x)
        & Map.unionsWith f -- or nested intmaps
        & Map.mapMaybe id
        & Map.foldrWithKey (\(di,dj) (si,sj) m' -> m' & at di . non mempty . at dj ?~ ()
                                        & at si . non mempty . at sj .~ Nothing) m
 where
 f (Just x) Nothing = Just x
 f Nothing (Just x) = Just x
 f _ _ = Nothing


propose1 stepN m (x,y) = do
  guard $ not isolated
  listToMaybe $ drop stepN $ cycle' $
      [ (x+dx,y) | dx <- [-1,1], dysEmpty (x+dx) ]
    ++[ (x,y+dy) | dy <- [1,-1], dxsEmpty (y+dy) ]
 where dysEmpty a = all (\dy -> hasn't (ix a . ix (y+dy)) m) [-1 .. 1]
       dxsEmpty a = all (\dx -> hasn't (ix (x+dx) . ix a) m) [-1 .. 1]
       isolated = and [ hasn't (ix (x+dx) . ix (y+dy)) m
                        | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx,dy) /= (0,0) ]

       cycle' [] = []
       cycle' x = cycle x
bb :: M -> Box V2 Int
bb m = m ^@.. ifolded <. S.members
  <&> (\(a,b) -> Box (V2 a b) (V2 a b))
  & foldr1 union

count :: M -> Int
count = sumOf (folded . to S.size)

-- seems to be correct based on input23se.txt
emptyGround m = 
 let Box a b =  bb m
     V2 x y = b - a
 in (x+1)*(y+1) - count m

ppM :: M -> IO ()
ppM m = let Box a b = bb m
            rg = [-3 .. 11]
            V2 nx ny = b - a
  in putStrLn $ unlines [ [ if has (ix i . ix j) m then '#' else '.' | j <- rg ]  | i <- rg ]


main = do
  inp <- readFile "inputs/input23s.txt" <&> toM . lines
  print $ emptyGround $ foldl step inp [0 .. 9]
  ppM (step inp 0)
  ppM (step (step inp 0) 1)
  print $ emptyGround inp
