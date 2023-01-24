module Day05 (main) where

import Data.List.Split
import Data.List
import Data.Char
import Control.Lens
import Control.Monad.Trans.State.Strict

import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Mutable as M
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Data.Int (Int8)

type CS = (M.IOVector (U.IOVector Int8), U.IOVector Int)

fromList maxL xs = do
  v <- V.new (fromMaybe (length xs) maxL)
  zipWithM_ (V.write v) [0 .. ] xs
  return v

mkCS cs = do
  v <- V.new (length cs)
  let maxL = Just (length (concat cs))
  zipWithM_ (V.write v) [0 .. ] =<< mapM (fromList maxL . map (fromIntegral . ord) . reverse) cs
  l <- fromList Nothing (map length cs)
  return (v,l)

applyMoveIO :: Bool -> CS -> [Int] -> IO ()
applyMoveIO rev (cs,ls) [n, srcIx, dstIx] = do
  src <- V.read cs srcIx
  dst <- V.read cs dstIx

  sN <- V.read ls srcIx
  dN <- V.read ls dstIx

  for_ [0 .. n] \ i -> do
    x <- V.read src (sN - i - 1)
    V.write src (sN - i - 1) 0 -- '\NUL'
    V.write dst (dN + if rev then n - i else i) x

  V.write ls srcIx (sN - (n+1))
  V.write ls dstIx (dN + (n+1))

summarizeCS :: CS -> IO String
summarizeCS (cs,ls) = for [0 .. V.length cs - 1] \i -> do
        j <- V.read ls i
        chr . fromIntegral <$> (flip V.read (pred j) =<< V.read cs i)
  
main = do
  (cs0, moves) <- parse <$> readFile "inputs/input5.txt"
  cs <- mkCS cs0
  mapM_ (applyMoveIO False cs) moves
  print =<< summarizeCS cs

  cs <- mkCS cs0
  mapM_ (applyMoveIO True cs) moves
  print =<< summarizeCS cs


applyMove rev [n, src, dst] = do
     vs <- ix src <<%= drop (n+1)
     ix dst %= (rev (take (n+1) vs) ++)

applyMoves f (arrWords, ms) = 
  map head $ execState (mapM_ (applyMove f) ms) arrWords

parse text = case splitOn "\n\n" text of
  [arr, moves] -> 
       (filter (not . null) $ map (filter isAlpha) $ transpose $ lines arr,
        map (map (subtract 1 . (read :: String -> Int)) .
                wordsBy (not . isDigit)) $ lines moves)
        
oldMain = do
  txt <- parse <$> readFile "inputs/input5.txt"
  print $ applyMoves reverse txt
  print $ applyMoves id txt
