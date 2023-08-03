-- https://atcoder.jp/contests/typical90/submissions/44213193
import Control.Monad
import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as BS
import Data.Array.ST
import Control.Monad.ST
import Data.STRef

readIntList :: IO [Int]
readIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine


solve :: Int -> [Int] -> [[Int]] -> [Int]
solve n as qs = runST $ do
  ds <- newListArray (0, n) as :: ST s (STUArray s Int Int)
  acc <- newSTRef (sum $ map abs as) :: ST s (STRef s Int)

  forM qs $ \[l,r,v] -> do
    le <- readArray ds (l-1)
    re <- readArray ds r
    let bef = abs le + abs re

    when (l > 1) $ do
      writeArray ds (l-1) (le+v) 

    when (r < n) $ do
      writeArray ds r (re-v)
    
    le' <- readArray ds (l-1)
    re' <- readArray ds r
    let aft = abs le' + abs re'

    modifySTRef acc (+ (aft - bef))
    readSTRef acc

calc :: [Int] -> [Int]
calc [] = []
calc (_:[]) = []
calc (x:x':xs) = (x' - x):(calc (x':xs))


main = do
  [n,q] <- readIntList 
  as <- readIntList 
  let as' = (0:(calc as)) ++ [0]

  qs <- replicateM q $ do
    [l,r,v] <- map read . words <$> getLine :: IO [Int]
    return [l,r,v]
 
  let ans = solve n as' qs 
  forM_ ans $ \a -> do
    print a
