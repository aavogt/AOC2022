module Day11b (main) where

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

data Monkey = Monkey { _num :: Int, _items :: [Integer],
        _op :: Integer -> Integer,
        _test :: Integer -> Bool,
        _dests :: (Int,Int),
        _divisor :: Integer }
$(makeLenses ''Monkey)

parse :: [String] -> Monkey
parse [stripPrefix "Monkey " -> Just (reads -> [(n, _)]),
        dropWhile (/=':') -> ':' : (splitOn "," -> (map read -> ns)),
        dropWhile (/=':') -> ':' : ' ' : optxt,
        dropWhile (/=':') -> ':' : ' ' : testtxt,
        read . last . words -> trueDest,
        read . last . words -> falseDest
        ] = Monkey n ns (parseOp optxt) (parseTest testtxt) (trueDest, falseDest) (divisibleBy testtxt)

parseTest (stripPrefix "divisible by " -> Just (read -> n)) =
  \ v -> mod v n == 0

divisibleBy (stripPrefix "divisible by " -> Just (read -> n)) = n

parseOp (stripPrefix "new = " -> Just (lex -> [(x, lex -> [(op, lex -> [(y, _)])])])) =
   \ old ->
        let f "old" = old
            f n = read n :: Integer
            (&) = case op of
                "+" -> (+)
                "*" -> (*)
        in f x & f y

monkeyOp :: Monkey -> IntMap Monkey -> IntMap Monkey
monkeyOp m ms = let (m1, m2) = partition (m^.test) $ map (m^. op) (m ^. items)
        in ms & ix (m^.dests._1) . items %~ (m1 ++ )
             & ix (m^.dests._2) . items %~ (m2 ++ )

main = do
  inp <- M.fromList . map ((\m -> (_num m, m)) . parse . lines) . splitOn "\n\n" <$> readFile "inputs/input11.txt"
  let passingRound = M.unionsWith (+) <$> for (M.keys inp) \ k -> do
                m <- gets (M.! k)
                modify (monkeyOp m)
                ix k . items .= []
                return $ M.singleton k (lengthOf (items.folded) m)
  let lcmMonkeys = inp ^.. folded . divisor
                & nub
                & product
  let reduction = mapped . items . mapped %= (`mod` lcmMonkeys)
  let part2 = sortOn (negate . snd) $ M.toList $ M.unionsWith (+) $ evalState 
                (replicateM 10000 (reduction >> passingRound)) inp
  print $ product $ map snd $ take 2 part2


