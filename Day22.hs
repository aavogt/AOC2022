{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Day22 (main) where

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
import Data.CircularList hiding (focus)
import qualified Data.CircularList
import Data.Char
import Data.List.Split
import Control.Monad.Trans.State
import Control.Lens
import Data.Maybe
import Data.Traversable

type M = CList (Int, CList (Int, Char))

focus :: Traversal' (CList a) a
focus f x = 
  case Data.CircularList.focus x of
    Nothing -> pure x
    Just v -> flip update x <$> f v

data DirA = R | D | L | U deriving (Enum, Show)
parseMap xs = fromList . zip [1 .. ] .  map (fromList . zip [1 .. ] . pad nc) . lines $ xs
  where
  emptyRow = fromList $ zip [1 .. ] $ replicate nc ' '
  pad n x = take n $ x ++ repeat ' '
  nc = maximum (map length (lines xs))

parseMv (span isDigit -> (n, xs)) = case n of
                [] | d:xs <- xs -> Right d : parseMv xs
                   | otherwise -> []
                _  | d:xs <- xs -> Left (read n :: Int) : Right d : parseMv xs

mvTurn (Left n) = do
  d <- use _1
  _2 %= mv n d
mvTurn (Right t) = _1 %= turn t
  

turn :: Char -> DirA -> DirA
turn 'R' = \case
  L -> U
  D -> L
  R -> D
  U -> R
turn 'L' = \d -> iterate (turn 'R') d !! 3
turn '\n' = id

mv :: Int -> DirA -> M -> M
mv n = wrapBlock n . mv1

mv1 = \case
  L -> mapped . _2 %~ rotL
  R -> mapped . _2 %~ rotR
  U -> rotL
  D -> rotR

ff2 :: Traversal' M Char
ff2 = focus . _2 . focus . _2

wrapBlock :: Int -> (M -> M) -> M -> M
wrapBlock 0 _ = id
wrapBlock n f = wrapBlock (n-1) f . tryMV (skippingBlank f)

tryMV :: (M -> M) -> M -> M
tryMV f x = let x' = f x
  in case x' ^? ff2 of
        Just '#' -> x
        _ -> x'

skippingBlank :: (M -> M) -> M -> M
skippingBlank f x = head $ dropWhile (has (ff2 . filtered (==' '))) $ iterate f (f x)

main = do
  [parseMap -> m, parseMv -> mvs] <- splitOn "\n\n" <$> readFile "inputs/input22.txt"
  print $ finalPW $ mapM_ mvTurn mvs `execState` (R, skippingBlank (mv1 R) m)
  putStrLn "part 2 not done"


finalPW (d, m) = (row,col, fromEnum d, 1000*row + 4*col + fromEnum d)
 where
 row = m ^?! focus . _1
 col = m ^?! focus . _2 . focus . _1


-- translate the cube into type M, keeping the original coordinates
-- should I number faces myself?
-- or do I sew the edges?
-- maybe I can just list the corners?
-- inside corners are easy: relative to the point
-- you mirror across the diagonal
--
-- corner at (cx,cy)
-- leave (x,y) going R
-- cy==y
-- enter (cx, cy + x-cx) going U
-- 
-- this covers 3 edges
-- 5 edges can be ignored because they are straight
-- 4 additional edges
-- 2-5, 2-1, 2-6, 1-6
-- 2-5
