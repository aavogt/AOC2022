module Day13 (main) where

import Data.List.Split
import Control.Lens
import Text.ParserCombinators.UU hiding (parse)
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import Data.List
import Data.Maybe

data T a = B [T a] | E a deriving (Show, Foldable, Traversable, Functor, Eq)

p1 :: Parser [T Int]
p1 = listParser ((B <$> p1) <|> (E <$> pNatural))

cmpTree (B xs) (B ys) = zcmpTree xs ys
cmpTree (E x) (B ys) = zcmpTree [E x] ys
cmpTree (B xs) (E y) = zcmpTree xs [E y]
cmpTree (E x) (E y) = compare x y

zcmpTree (y:ys) [] = GT
zcmpTree [] (y:ys) = LT
zcmpTree (x:xs) (y:ys) = cmpTree x y <> zcmpTree xs ys
zcmpTree [] [] = EQ

parse :: String -> T Int
parse = runParser "inputs/input" (B <$> p1)

sumLTIndices xs = xs ^@.. folded . filtered (== LT) <&> succ . fst & sum

main = do
  parsed <- map (map parse . lines) . splitOn "\n\n" <$> readFile "inputs/input13.txt"
  print $ sumLTIndices $ map (\[x,y] -> cmpTree x y) parsed
  let dividers = parse <$> ["[[2]]","[[6]]"]
  let sorted = sortBy cmpTree $ concat $ dividers : parsed
  print $ product $ map succ $ fromJust $ mapM (`elemIndex` sorted) dividers
