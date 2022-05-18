-- https://atcoder.jp/contests/typical90/submissions/31784772
import Data.Char
import Data.List as L
import Data.Set as S
import Control.Monad
import qualified Data.ByteString.Char8 as BS

calc :: [Int] -> Int -> Int
calc as b = min (abs $ l - b) (abs $ r - b)
  where
    (Just l)  = lookupLE b $ S.fromList as
    (Just r) = lookupGT b $ S.fromList as

readIntList :: IO [Int]
readIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main = do
  _ <- getLine
  as <- readIntList 
  q <- readLn
  bs <- replicateM q readLn 

  let as' = sort $ -(10^10):(as) ++ [10^10]
  let ans = L.map (calc as') bs

  forM_ ans $ \a -> do  
    print a
