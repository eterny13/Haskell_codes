-- http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=3386593#1
-- This code can solve all data sets by using Data.Set
import qualified Data.Set as S

solve :: [Int] -> [Int] -> Int
solve ss ts = S.size $ S.intersection ss' ts' 
  where
    ss' = S.fromList ss
    ts' = S.fromList ts

  
readInt :: String -> Int
readInt = read

main = do
  n <- getLine 
  s <- fmap (map readInt . words) getLine 

  q <- getLine 
  t <- fmap (map readInt . words) getLine 

  let ans = solve s t 
  print ans
