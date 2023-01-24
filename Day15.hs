{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Day15b (main) where


import Data.List.Split
import Data.Char
import Control.Monad
import Control.Lens hiding ((.>))
import Data.Either
import Data.List
import Data.SBV hiding ((%))
import Data.SBV.Internals (CV(cvVal), SMTModel (modelAssocs))
import Data.SBV.Dynamic (CV(..), CVal (CInteger))
import Data.Foldable
import Data.Maybe

mdt [x,y, a,b] = [x,y, abs (x-a) + abs (y-b)]

yCut = 2000000

type I = (Int,Int)

yInt :: [Int] -> I
yInt [x,y,d] =
  let dx = max 0 $ d - abs (y - yCut)
  in (x - dx, x + dx)

(%) :: I -> Int -> Bool
(%) (lx,rx) y = y >= lx && y <= rx

fullyContains :: I -> I -> Bool
fullyContains x (ly,ry) = x % ly && x % ry

overlap :: I -> I -> Maybe I
overlap x@(lx,rx) y@(ly,ry) =
  if | x `fullyContains` y -> Just x
     | y `fullyContains` x -> Just y
     | x%ly || x%ry || y%lx || y%rx -> Just (min lx ly, max rx ry)
     | otherwise -> Nothing

ro :: [I] -> [I]
ro (x : xs) = case firstJust (overlap x) xs of
  Nothing -> x : ro xs
  Just (x', xs) -> ro (x' : xs)
ro [] = []

firstJust :: (a -> Maybe a) -> [a] -> Maybe (a,[a])
firstJust f (x : xs) = case f x of
        Nothing -> firstJust f xs <&> _2 %~ (x:)
        Just a -> Just (a, xs)
firstJust _ _ = Nothing

size :: I -> Int
size = uncurry subtract

main = do
  inp <- map mdt . chunksOf 4 . map (\x -> read x :: Int)
                . wordsBy (\c -> not (isDigit c || c == '-'))
                <$> readFile "inputs/input15.txt"
  print $ sum $ map size $ ro $ sortOn fst $ filter ((/=0) . size) $ map yInt inp
  print =<< notInside inp

notInside (map (map fromIntegral) -> xyds) = (toDistress <$>) $ sat do
        x <- sInteger "x"
        y <- sInteger "y"
        constrain $ x .<= 4000000
        constrain $ y .<= 4000000
        constrain $ x .>= 0
        constrain $ y .>= 0
        for_ xyds \ [xi,yi,d] ->
          constrain $ abs (xi - x) + abs (yi - y) .> d
  where
  toDistress sol = var "x" * 4000000 + var "y" where
    var n = fromJust $ lookup n assocs
    assocs = case sol of
        SatResult (Satisfiable _ m) -> [ (k, n) | (k, CV _ (CInteger n)) <- modelAssocs m ]
  
