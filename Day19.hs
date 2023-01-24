{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
module Day19 (main) where

import Control.Lens hiding ((.>))
import Data.List.Split
import Data.Char
import Data.SBV
import Control.Monad
import Data.List
import Data.Foldable
import Control.Monad.Trans.State
import Control.Applicative
import Data.SBV.Internals

data BP a = BP { coro, corc, corb, ccrb, corg, cbrg :: a }
 deriving (Show, Functor)


toBP [_, a,b,c,d,e,f] = BP a b c d e f

toF :: Int -> [SInt8] -> [SInt8]
toF i = map (fromIntegral i*)


getVal (IndependentResult [(_, Satisfiable _ m)]) = case modelObjectives m of
  [(_, RegularCV (CV _ (CInteger x)))] -> x

sol step (fmap toF -> BP{ .. }) = optimize Independent $ do
  -- robots
  ro <- replicateM step $ sInt8 "ro"
  rc <- replicateM step $ sInt8 "rc"
  rb <- replicateM step $ sInt8 "rb"
  rg <- replicateM step $ sInt8 "rg"
  -- resources
  o <- replicateM step $ sInt8 "o"
  c <- replicateM step $ sInt8 "c"
  b <- replicateM step $ sInt8 "b" -- oBsidian
  g <- replicateM step $ sInt8 "g"
  -- equations for conservation
  let diff xs = zipWith (-) (drop 1 xs) xs
  let dro = diff ro
      drc = diff rc
      drb = diff rb
      drg = diff rg
      d_o = diff o
      dc = diff c
      db = diff b
      dg = diff g

  -- initial robots, resources
  constrain (head ro .== 1)
  for_ [rc,rb,rg,o,c,b,g] \ r -> constrain (head r .== 0)

  -- only make one robot at a time
  sequence_ $ zipWith4 (\a b c d -> constrain $ a+b+c+d .<= 1) dro drc drb drg
  for_ [dro,drc, drb, drg] \ drx -> 
        for_ drx \x -> do
          constrain (x .>= 0)
          constrain (x .<= 1)

  -- cannot use resources you don't have
  for_ [o,c,b,g] \ rs -> for_ rs \ x -> constrain (x .>= 0)

  -- resources used to make robots
  let oUse = coro dro + corc drc + corb drb + corg drg
        where (+) = zipWith (Prelude.+)
      cUse = ccrb drb
      bUse = cbrg drg

      payFirst resource consumption = mapM_ constrain $ zipWith (.>=) resource (consumption ++ [0])

  payFirst o oUse
  payFirst c cUse
  payFirst b bUse

  let bal resource robots use = mapM_ constrain $ zipWith (.==) resource (zipWith (-) robots use)

  bal d_o ro oUse
  bal dc rc cUse
  bal db rb bUse
  bal dg rg (repeat 0)

  maximize "geodes" (sFromIntegral (last g) :: SInteger)


main = do
  inp <- map (toBP . map (read :: String -> Int) . wordsBy (not . isDigit)) . lines <$> readFile "inputs/input19.txt"
  -- part 1
  -- print . sum . zipWith (*) [1 .. ] . map getVal =<< mapM (sol 25) inp

  print . product . map getVal =<< mapM (sol 33) (take 3 inp)
  {-
  for_ [25 .. 30] \ x -> do
     print =<< sol x (inp !! 0)
     print x
     -}

{-

-- or if the IP doesn't work
-- how does the simulation work?
-- there is a path through the Int^8 that has a maximum
-- 8th component
data R a = R { _ro, _rc, _rb, _rg :: a } deriving (Functor)
instance Applicative R where
  pure a = R a a a a
  R a b c d <*> R w x y z = R (a w) (b x) (c y) (d z)

$(makeLenses ''R)
type S = (R Int,R Int) -- robots, resources
type M = State S

step :: M ()
step = do
  robots <- use _1

  -- collect resources
  _2 %= liftA2 (+) robots
  return ()
  -}
