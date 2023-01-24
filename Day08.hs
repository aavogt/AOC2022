module Day08 (main) where

{-# LANGUAGE LambdaCase #-}

import qualified Data.Set as S
import Control.Lens
import Data.List
import Data.Maybe

split = map (map (\x -> read [x] :: Int)) . lines

highPointIxes :: [Int] -> [Int]
highPointIxes xs = findIndices (\case
        x@(_:_) -> all (last x>) (init x)
        ) $ drop 1 $ inits xs

highPoints2 :: [[Int]] -> [(Int,Int)]
highPoints2 xs = concat $
          zipWith (\i x -> map (i,) (highPointIxes x)) [0 .. ] xs
       ++ zipWith (\i x -> map (,i) (highPointIxes x)) [0 .. ] (transpose xs)

highPoints3 xs = highPoints2 xs ++ map correct (highPoints2 (reverse (map reverse xs)))
  where correct (x,y) = (n-x, n-y)
        n = length xs - 1

pp :: Int -> [(Int,Int)] -> IO ()
pp r ijs = putStrLn $ unlines [ [ if (i,j) `elem` ijs then '#' else ' ' | i <- [0 .. r] ] | j <- [0 .. r] ]

viewingDistance (x:xs) = maybe (length xs) (+1) $ findIndex (x <=) xs

lookupAr arr ixes = map (\(i,j) -> arr !! i !! j) ixes

mkIxes :: (Int -> Bool) -> (Int,Int) -> [[(Int,Int)]]
mkIxes inBounds (i,j) =
   [ takeWhile (allOf each inBounds) $ iterate f (i,j) |
     f <- [ _1 +~ 1,
            _1 -~ 1,
            _2 +~ 1,
            _2 -~ 1 ]
      ]

main = do
  f <- split <$> readFile "inputs/input8.txt"
  let ps = nub $ highPoints3 f
  print ("part 1" , length ps)
  let score ij = product
                $ map (viewingDistance . lookupAr f)
                $ mkIxes (\ i -> i >= 0 && i < length f) ij
      allIndices = [ (i,j) | i <- [0 .. length f - 1], j <- [0 .. length f - 1 ] ]
  print ("part 2", maximum $ map score allIndices)
