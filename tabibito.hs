-- https://atcoder.jp/contests/joi2010ho/submissions/26674713
import Control.Monad

solve :: Int -> [Int] -> [Int] -> [Int]
solve _ _ [] = []
solve now ds (x:xs) = ss `seq` ss : (solve to ds xs)
  where 
    to = now + x 
    ss = abs $ head $ zipWith (-) (drop (to-1) ds) (drop (now-1) ds)


main = do
  [n, m] <- map read . words <$> getLine

  dist <- replicateM (n-1) readLn 
  ts <- replicateM m readLn 

  let dist' = scanl (+) 0 dist

  let ans = (sum $ solve 1 dist' ts) `mod` 10^5

  print ans