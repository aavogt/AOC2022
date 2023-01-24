module Day04 (main) where

import Data.List.Split


parse :: String -> [Int]
parse = concatMap (map read . splitOn "-") . splitOn ","

fullyContains :: [Int] -> Bool
fullyContains [a,b,c,d] = (within a b c && within a b d) ||
        within c d a && within c d b

anyoverlap :: [Int] -> Bool
anyoverlap [a,b,c,d] = within a b c || within a b d || within c d a || within c d b

within a b c = b >= c && c >= a

main = do
  rgs <- map parse . lines <$> readFile "inputs/input4.txt"
  putStrLn "part1"
  print $ length $ filter fullyContains rgs
  putStrLn "part2"
  print $ length $ filter anyoverlap rgs
