-- https://atcoder.jp/contests/abc150/tasks/abc150_c
import Data.List

solve ct n [] = ct 
solve ct n (x:xs) = 
  if n == x then ct 
  else solve (ct+1) n xs

main = do
  n <- readLn
  p <- map read . words <$> getLine :: IO [Int]
  q <- map read . words <$> getLine :: IO [Int]
  let nl = [1..n]
  let pidx = solve 1 p (sort $ permutations nl)
  let qidx = solve 1 q (sort $ permutations nl)

  print $ abs $ pidx-qidx
