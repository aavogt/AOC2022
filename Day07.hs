{-# LANGUAGE OverloadedStrings #-}
module Day07 (main) where
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.Trans.State
import Control.Lens hiding (children)

import Data.Tree.Zipper
import Data.Maybe
import Data.List hiding (delete, insert)
import Data.Tree
import Control.Monad
import Data.Char (isDigit)
import Data.Either
import Data.Traversable
import Data.Foldable


type S = (TreePos Full (L.ByteString, Maybe Int))


splitSize x = case L.span isDigit x of
  ("", L.drop 4 -> dir)  -> (dir, Nothing)
  (sz, L.drop 1 -> file) -> (file, fmap fst (L.readInt sz))

runCommand :: (L.ByteString, [L.ByteString]) -> State S ()
runCommand ("$ ls", map splitSize -> result) = modify $ addChildren result
runCommand (L.stripPrefix "$ cd " -> Just dest, _)
        | ".." == dest = modify (fromJust . parent)
        | "/" == dest = modify root
 | otherwise = do
  -- ensure it exists
  modify $ addChildren [(dest, Nothing)]
  modify $ fromJust . findChild dest

lab :: Lens' (TreePos Full a) a
lab = lens label (flip setLabel)

treeT :: Lens' S (Tree (L.ByteString, Maybe Int))
treeT f x = flip setTree x <$> f (tree x)

rootL :: Lens' (Tree a) a
rootL f (Node x xs) = flip Node xs <$> f x

subF :: Lens' (Tree a) (Forest a)
subF f (Node x xs) = Node x <$> f xs

childLabels :: Traversal' S (L.ByteString, Maybe Int)
childLabels = treeT . subF . traversed . rootL

addChildren :: [(L.ByteString, Maybe Int)] -> S -> S
addChildren ls s = let missing = [ Node x [] | x <- ls \\ (s ^.. childLabels) ]
  in foldr (\x -> fromJust . parent . insert x . children) s missing

-- for huge input, I would sort the children
findChild   :: L.ByteString -> S -> Maybe S
findChild n s = find nameMatches $ catMaybes $ takeWhile isJust $ iterate (next =<<) (firstChild s)
 where nameMatches = (==n) . fst . label

chunk = L.groupBy (\ _ x -> '$' /= x)

split x = case L.lines x of
  c:output -> (c,output)

main = do
  cs <- map split . chunk <$> L.readFile "inputs/input7.txt"
  let fs = mapM runCommand cs `execState` fromTree (Node ("/", Nothing) [])
  let fsAnot = annotateSize $ toTree $ root fs
  print $ sum $ fsAnot ^.. folded . filtered (view _3) . _2 . filtered (<= 100000)
  let needed    = 30000000
      total     = 70000000
      currentlyFree = total - rootSize
      minDelete = needed - currentlyFree
      rootSize  = rootLabel fsAnot ^. _2

  print $ listToMaybe $ dropWhile (<=minDelete) $ sort $ fsAnot ^.. folded . _2

annotateSize :: Tree (L.ByteString, Maybe Int) -> Tree (L.ByteString, Int, Bool)
annotateSize (Node (a,Just x) []) = Node (a,x, False) []
annotateSize (Node (a, _) xs) = Node (a, sumOf (folded . rootL . _2) xs', True) xs'
 where xs' = map annotateSize xs

