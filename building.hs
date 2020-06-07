-- https://atcoder.jp/contests/s8pc-4/tasks/s8pc_4_b
import Control.Monad

solve :: Int -> [Integer] -> Integer -> Integer -> Integer
solve 0 as hd cnt = cnt
solve _ [] _ _ = 10^18 
solve k (a:as) hd cnt
  | hd < a = solve (k-1) as a cnt
  | otherwise = solve k as hd cnt `min` solve (k-1) as (hd+1) (cnt+hd+1-a)


main = do
  [n,k] <- map read . words <$> getLine
  a <-  map read . words <$> getLine :: IO [Integer]

  let ans = solve (k-1) (tail a) (head a) 0
  print ans
