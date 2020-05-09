-- atcoder   https://atcoder.jp/contests/abc128/tasks/abc128_c
import Control.Monad
import Data.Bits

calc :: [[Int]] -> [Int] -> [Int] -> Int
calc ks ps ns = length . filter (==True) $ map allOk ns 
  where
    allOk n = all (\(k,p) -> calc' k p n) (zip ks ps)


calc' :: [Int] -> Int -> Int -> Bool
calc' k p n = jdg
  where 
    ct = length . filter (==1) $ map (\e ->  (shift n (1-e)) `mod` 2) k
    jdg = if ct`mod`2 == p then True else False
    

main = do
  [n,m] <- map read . words <$> getLine
  k <- replicateM m $ tail . map read . words <$> getLine :: IO [[Int]]
  p <- map read . words <$> getLine :: IO [Int]
  let ans = calc k p [0..2^n-1]
  print ans
