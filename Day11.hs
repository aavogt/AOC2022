module Day11 (main) where

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
import Data.List.Split
import Control.Lens hiding (op)
import Data.IntSet (IntSet)
import Data.List
import qualified Data.Sequence as S
import Data.Sequence (Seq)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Control.Monad.Trans.State
import Data.Foldable
import Data.Traversable
import Control.Monad

data Monkey = Monkey { _num :: Int, _items :: [Int],
        _op :: Int -> Int,
        _test :: Int -> Bool,
        _dests :: (Int,Int) }
$(makeLenses ''Monkey)

parse :: [String] -> Monkey
parse [stripPrefix "Monkey " -> Just (reads -> [(n, _)]),
        dropWhile (/=':') -> ':' : (splitOn "," -> (map read -> ns)),
        dropWhile (/=':') -> ':' : ' ' : optxt,
        dropWhile (/=':') -> ':' : ' ' : testtxt,
        read . last . words -> trueDest,
        read . last . words -> falseDest
        ] = Monkey n ns (parseOp optxt) (parseTest testtxt) (trueDest, falseDest)

parseTest (stripPrefix "divisible by " -> Just (read -> n)) =
  \ v -> mod v n == 0

parseOp (stripPrefix "new = " -> Just (lex -> [(x, lex -> [(op, lex -> [(y, _)])])])) =
   \ old ->
        let f "old" = old
            f n = read n :: Int
            (&) = case op of
                "+" -> (+)
                "*" -> (*)
        in f x & f y

monkeyOp :: Monkey -> IntMap Monkey -> IntMap Monkey
monkeyOp m ms = let (m1, m2) = partition (m^.test) $ map ((`div` 3) . (m^. op)) (m ^. items)
        in ms & ix (m^.dests._1) . items %~ (m1 ++ )
             & ix (m^.dests._2) . items %~ (m2 ++ )

main = do
  inp <- M.fromList . map ((\m -> (_num m, m)) . parse . lines) . splitOn "\n\n" <$> readFile "inputs/input11.txt"
  let passingRound = M.unionsWith (+) <$> for (M.keys inp) \ k -> do
                m <- gets (M.! k)
                modify (monkeyOp m)
                ix k . items .= []
                return $ M.singleton k (lengthOf (items.folded) m)
  let part1 = sortOn (negate . snd) $ M.toList $ M.unionsWith (+) $ evalState (replicateM 20 passingRound) inp
  print $ product $ map snd $ take 2 part1
