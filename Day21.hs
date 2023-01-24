module Day21 (main) where

-- 11:36 start, 12:26 done
{-# LANGUAGE LambdaCase #-}
import qualified Data.Map as M
import Data.List.Split
import Data.Map (Map)
import Control.Monad.Trans.State
import Control.Lens
import Text.Show.Pretty
import Data.SBV
import Control.Applicative

type Env = Map String (Either Int [String])

parse :: String -> Env
parse xs = case splitOn ":" xs of
  [v, bo] -> M.singleton v (parseBody bo)

parseBody xs = case words xs of
  [a,op,b] -> Right [a,op,b]
  [v] -> Left (read v)

sEv :: String -> State Env (Symbolic SInteger)
sEv "humn" = return (sInteger "humn")
sEv s = do
  ~(Just v) <- use (at s)
  case v of
    Left n -> return (return (fromIntegral n))
    Right [a,op,b] -> liftA2 (sOp op) <$> sEv a <*> sEv b

sOp = \case
   "*" -> (*)
   "/" -> sDiv
   "+" -> (+)
   "-" -> (-)

main = do
  m <- M.unions . map parse . lines <$> readFile "inputs/input21.txt"
  print $ ev "root" `evalState` m

  let m2 = m & ix "root" . _Right %~ \ [a,_,b] -> [a,"-", b]
  result <- sat $ do
    v <- sEv "root" `evalState` m2
    constrain $ v .== 0
  print result

ev :: String -> State Env Int
ev s = do
  ~(Just v) <- use (at s)
  case v of
    Left n -> return n
    Right [a,op,b] -> do
        n <- evOp op <$> ev a <*> ev b
        at s ?= Left n -- avoid recomputing 's', but it makes no difference
        return n

evOp = \case
   "*" -> (*)
   "/" -> div
   "+" -> (+)
   "-" -> (-)
