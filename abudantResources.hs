// https://atcoder.jp/contests/nikkei2019-final/submissions/26427775
import Control.Monad

solve :: Int -> [Int] -> [Int]
solve n xs = [maximum $ zipWith (-) (drop i xs) xs | i <- [1..n]]

main = do
  n <- readLn
  as <- map read . words <$> getLine :: IO [Int]

  let as' = scanl (+) 0 as 

  let ans = solve n as' 

  forM_ ans $ \i -> do
    print i
