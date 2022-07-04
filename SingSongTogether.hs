-- https://atcoder.jp/contests/typical90/submissions/32985632
import Data.List

main = do
  _ <- getLine

  as <-  map read . words <$> getLine :: IO [Int]
  bs <-  map read . words <$> getLine :: IO [Int]

  let as' = reverse $ sort as
  let bs' = reverse $ sort bs
  
  let ans = sum $ map (abs) $ zipWith (-) as' bs'
  print ans
