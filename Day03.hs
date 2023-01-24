module Day03 (main) where

import Data.Char
import Data.Maybe
import Data.List
import Debug.Trace
import Data.List.Split

sp xs = splitAt (length xs `div` 2) xs

priority x = fromJust $ lookup x $ (['a' .. 'z'] ++ ['A' .. 'Z' ]) `zip` [1 .. ]

f = sum . map priority . nub . uncurry intersect . sp

main = do
  print . sum . map f . lines =<< readFile "inputs/input3.txt"

  print . sum . map (sum . map priority . nub . foldr1 intersect) . chunksOf 3 . lines =<< readFile "inputs/input3.txt"
