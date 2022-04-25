-- https://atcoder.jp/contests/typical90/submissions/31269324
import Control.Monad
import Data.Char
import Data.List
import Data.Array
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString.Char8 as BS


solve :: Int -> Int -> Array (Int,Int) Int -> UV.Vector Int -> UV.Vector Int -> [Int]
solve h w as vs hs = [ (vs UV.! i) + (hs UV.! j) - (as ! (i,j)) | i <- [0..h], j <- [0..w]]

loop :: Int -> Int -> [Int] -> [[Int]]
loop 1 _ xs  = [xs]
loop h w xs = ((take w xs):(loop (h-1) w (drop w xs)))

readIntList :: IO [Int]
readIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main = do
  [h,w] <- readIntList
  as <- replicateM h readIntList

  let vsum = UV.fromList $ map sum as
  let hsum = UV.fromList $ map sum $ transpose as

  let vas = UV.concat $ map (UV.fromList) as
  let as' = listArray ((0,0), (h-1,w-1)) $ UV.toList vas
  let ans = loop h w $ solve (h-1) (w-1) as' vsum hsum

  forM_ ans $ \xs -> do
    putStrLn $ unwords $ map show xs

