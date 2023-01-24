module Day20 (main) where

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE ImplicitParams #-}
import Data.CircularList
import Data.Maybe

-- oddly if I change mix to operate on CList (Int,Int)
-- rather than involve Maybe (CList (Int,Int)) runtime goes
-- from 13s to 41s, and I could not figure out how to reintroduce
-- strictness lost by adding fromJust
nextTarget a = findRotateTo ((a ==) . fst)

send x = do
  (a, v) <- focus x
  Just $ insertR (a, v) $ rotN (v `mod` ?n) (removeR x)

mix xs0 = fromJust $
   foldl (\xs n -> xs >>= nextTarget n >>= send) (Just xs0) [ 0 .. ?n ]

printGroveSum xs =
  let Just r = findRotateTo (==0) (snd <$> xs)
  in print $ sum <$> sequence [focus (rotN j r) | j <- [1000, 2000, 3000] ]

main = do
  inp <- map read . lines <$> readFile "inputs/input20.txt"
  let ?n = length inp - 1
  printGroveSum $ mix $ fromList $ zip [0 .. ] inp

  let k = 811589153
  let mixN n v = iterate mix v !! n
  printGroveSum $ mixN 10 $ fromList $ zip [0 .. ] (map (k*) inp)
