module Day01 (main) where

import Data.List.Split
import Data.List
main = do
  cs <- readFile "inputs/input1.txt"
  let es = map (map (\x -> read x :: Int) . lines) $ splitOn "\n\n" cs
  putStrLn "part 1"
  print $ last $ sortOn snd $ zip [0 .. ] $ map sum es
  putStrLn "part 2"
  print $ sum $ map snd $ take 3 $ reverse $ sortOn snd $ zip [0 .. ] $ map sum es
  return ()
