module Day10 (main) where

import Data.List.Split

main = do
  cmds <- map words . lines <$> readFile "inputs/input10.txt"
  let op (x:xs) ["noop"] = x:x:xs
      op (x:xs) ["addx", n] = (x+read n):x:x:xs

  let ix0 = [ 20, 60, 100, 140, 180, 220]
      allCycles = reverse $ foldl op [1] cmds
  print $ sum [ i * allCycles !! (i-1) | i <- ix0 ]

  let rd i = mod (i - 1) 40 + 1
  -- works well enough but the very first column is . but
  -- should be '#': 
  -- .##...##..###..###..#..#..##..###....##.
  -- ...#.#..#.#..#.#..#.#.#..#..#.#..#....#.
  -- ...#.#....#..#.###..##...#..#.#..#....#.
  -- .##..#....###..#..#.#.#..####.###.....#.
  -- .....#..#.#....#..#.#.#..#..#.#....#..#.
  -- ......##..#....###..#..#.#..#.#.....##..
  putStrLn
        $ unlines
        $ chunksOf 40
        $ zipWith (\ i x -> if abs (rd i-x) < 2 then '#' else '.')
                [0 .. 240 - 1]
                allCycles
