{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Day16 (main) where

import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Char
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Trans.State
import Control.Lens
import Data.List
import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.Tuple
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.IntMap (IntMap)

import Data.Foldable
import Data.Traversable
import Debug.Trace
import Text.Show.Pretty
import Data.Maybe
import Control.Monad.Trans.Class

newtype Flow = Flow Int deriving (Show, Eq)
type Caves = IntMap (Flow, [Int])
data S = S { _cur :: [Int],
             _flow,
             _flowed,
             _time, _timeEnd :: !Int,
             _open :: IntSet,
    _objs :: [String],
    _others :: Int -> [(Int,Flow,Int)] }
type M = StateT S Logic
makeLenses 'S
makeWrapped 'Flow

main = do
  Just x <- mapM (match pLine) . sort . lines <$> readFile "inputs/input16.txt" 

  let iToN = M.fromList $ zipWith (\ i (k,_,_) -> (i,k)) [0 .. ] x
      nToI = Map.fromList $ map swap $ M.toList iToN

      caves :: Caves
      caves = x <&> _1 %~ (nToI Map.!)
                <&> _2 %~ Flow
                <&> _3 . mapped %~ (nToI Map.!)
          <&> (\(a,b,c) -> (a,(b,c)))
            & M.fromList

  let otherValves :: Int -- start
                -> [(Int, Flow, Int)] -- (end,flow,hops)
      otherValves n = dijkstra n caves ^@.. ifolded . filtered (views _1 (Flow 0 /=))
                & sortOn (^. _2 . _3) -- lowest hops first
                & nub
                <&> \(end, (f, _neighs, hops)) -> (end, f, hops)
      x:_ = runM (S [0] 0 0 0 30 (getZeroFlows caves) [] otherValves) moveAndOpens 
  putStrLn "part 1"
  print $ x^.flowed

  -- too slow because the objective is strictly evaluated?
  let human = runM (S [0] 0 0 0 26 (getZeroFlows caves) [] otherValves) moveAndOpens
      together = maximum [ elephant + s^.flowed
                        | s <- human,
                        let elephant = runM (S [0] 0 0 0 26 (s ^. open) [] otherValves) moveAndOpens ^?! _head . flowed]
      
  putStrLn "part2 not completed"
  -- print together


getZeroFlows :: Caves -> IntSet
getZeroFlows vs = vs ^@.. ifolded <. _1 . _Wrapped . filtered (==0) <&> fst
        & S.fromList

type CavesD = IntMap (Flow, [Int], Int)

dijkstra :: Int -- ^ starting cave
        -> Caves -- ^ map
        -> CavesD -- ^ map with distances from the starting cave
dijkstra n vs = spreadD mempty (M.singleton 0 [n]) (addD vs & ix n . _3 .~ 0)
 where
  addD :: Caves -> CavesD
  addD vs = vs <&> \(a,b) -> (a,b,maxBound)
  
  spreadD :: IntSet -> IntMap [Int] -> CavesD -> CavesD
  spreadD seen working vs | M.size working > 0 = case M.deleteFindMin working of
        ((_, []), working') -> spreadD seen working' vs
        ((d, s:ss), working') -> let ns = S.toList $ S.fromList (vs ^.. ix s . _2 . folded) S.\\ seen
                                in spreadD (S.insert s seen) (M.insert d ss $ M.insertWith (++) (d+1) ns working') $
                                  foldl (\ vs n -> vs & ix n . _3 %~ min (d+1))
                                          vs 
                                          ns
  spreadD _ _ vs = vs


advanceTime n = do
  tE <- use timeEnd
  ot <- time <<%= min tE . (n+)
  f <- use flow
  flowed += f* min n (tE - ot)

-- move into another module and see about getting ghcid to load compiled code
moveAndOpens ::  M ()
moveAndOpens = do
  [i] <- use cur
  op <- use open
  dests <- uses others (filter ((`S.notMember` op) . view _1) . ($ i))
  tE <- use timeEnd
  if null dests then do
        t <- use time
        when (t < tE) $ advanceTime (tE+1)
       else do
    (j, Flow fj, dj) <- foldr1 (<|>) $ map return dests
    cur .= [j]
    open . at j ?= ()
    advanceTime (dj+1)
    flow += fj
    f <- use flow
    t <- use time
    when (t < tE) moveAndOpens

-- part 2 involves two searches over a 26 minutes
-- small gives 1707
-- either have simultaneous searches,
-- or do them successively: that is,
-- the elephant goes,
-- then the human goes reusing openValves
-- total pressure is the sum of both


mo2 :: M2 ()
mo2 = do
  [i,j] <- use cur
  op <- use open
  dests <- uses others (filter ((`S.notMember` op) . view _1) . ($ i))
  tE <- use timeEnd
  let ubTooLow = do
        f <- use flow
        fd <- use flowed
        t <- use time
        let fs = dests ^.. folded . _2 . _Wrapped & sort & reverse
        let relaxed = fd + (tE-t)*f + undefined
        best <- lift (lift get)
        return $ best >= relaxed
  tooLow <- ubTooLow
  if null dests || tooLow then do
        t <- use time
        when (t < tE) $ advanceTime (tE+1)
       else do
    (j, Flow fj, dj) <- foldr1 (<|>) $ map return dests
    return ()
  return ()



select :: [a] -> [(a,[a])]
select (x:xs) = (x,xs) : (select xs <&> _2 %~ (x:))

-- how do I branch and bound?
-- one relaxation is to say that the two searchers can open the same
-- valve and the total flow will be larger
-- so if the flow in that case is still lower than the best,
-- that option can be eliminated
-- so I need a state common to all
type M2 = StateT S (LogicT (State Int))
 
runM2 :: S -> M2 a -> Int
runM2 s0 = (execState ?? 0) . observeAllT . (execStateT ?? s0)

pV = many (psym isSpace) *> replicateM 2 anySym
        <* many (psym isSpace)

pVs = (:) <$> pV <*> many (", " *> pV)

pLine = (,,) <$> ("Valve" *> pV <* "has flow rate=")
        <*> (decimal <* ";" <* many (psym isLower <|> psym isSpace)) <*>
        pVs

runM s0 = sortOn (^. flowed . to negate) . observeAll . (execStateT ?? s0)
