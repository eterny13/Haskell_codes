-- Union Find Tree
-- https://atcoder.jp/contests/atc001/submissions/28466481
import Control.Applicative
import Control.Monad
import Data.List

import Control.Monad.ST
import Data.Array.ST
import Data.STRef

newtype UFind s = UFind (STUArray s Int Int)

newUF :: (Int, Int) -> ST s (UFind s)
newUF (i, j) = liftM UFind (newListArray (i, j) [i .. j])

rootUF :: UFind s -> Int -> ST s Int
rootUF (UFind a) i = do
  p <- readArray a i
  if p == i
    then return p
    else do
      rp <- rootUF (UFind a) p
      writeArray a i rp
      return rp

findUF :: UFind s -> Int -> Int -> ST s Bool
findUF uf x y = liftM2 (==) (rootUF uf x) (rootUF uf y)

uniteUF :: UFind s -> Int -> Int -> ST s ()
uniteUF uf@(UFind a) x y = do
  rx <- rootUF uf x
  ry <- rootUF uf y
  when (rx /= ry) $ do
    writeArray a ry rx
  

solve :: Int -> [(Int, Int, Int)] -> [String]
solve n qs = runST $ do
  uf <- newUF (0, n)
  res <- newSTRef [] 
  forM_ qs $ \(p, a, b) -> do
    case p of
      0 -> uniteUF uf a b
      1 -> do
        same <- findUF uf a b
        case same of
          True -> modifySTRef res ("Yes":)
          False -> modifySTRef res ("No":)
  readSTRef res


main = do
  [n, q] <- map read . words <$> getLine
  let f [p,a,b] = (p,a,b)
  qs <- replicateM q $ f . fmap read .words <$> getLine
  let ans = reverse $ solve n qs

  forM_ ans $ \i -> do
    putStrLn i
    
