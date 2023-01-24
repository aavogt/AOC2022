{-# LANGUAGE LambdaCase #-}
module Day02 (main) where



shapeScore = \case
        "A" -> 1
        "B" -> 2
        "C" -> 3

abc = words "A B C"

myDraw2 a result = head [ m | m <- abc,
        win a m == case result of
           "X" -> 0
           "Y" -> 3
           "Z" -> 6 ]

myDraw1 = \case
        "X" -> "A"
        "Y" -> "B"
        "Z" -> "C"

win 
  a x | a == x = 3
win "B" "A" = 0
win "C" "B" = 0
win "A" "C" = 0
win  _ _ = 6

score2 [a,x] = let b = myDraw2 a x
   in win a b + shapeScore b

score1 [a,x] = let b = myDraw1 x
   in win a b + shapeScore b


main = do
  x <- readFile "inputs/input2.txt"
  print . sum . map (score1 . words) $ lines x
  print . sum . map (score2 . words) $ lines x
