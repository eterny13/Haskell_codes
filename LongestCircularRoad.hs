-- https://atcoder.jp/contests/typical90/submissions/31102575
import Data.Array
import Control.Monad

solve :: Int -> [(Int,Int)] -> Int
solve n abs = 1 + (fst $ maximum $ search 0 dv dv [])
  where
    g = accumArray (flip (:)) [] (1,n) $ concatMap (\(a,b) -> [(a,b),(b,a)]) abs
    dv = snd $ maximum $ search 0 1 1 []
    search :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
    search dep par id xs = chr ++ nxt
      where
        es = filter (/=par) $ g ! id
        nxt = foldr (search (dep+1) id) xs es
        chr = map (\i -> (dep+1, i)) es 


main = do
  n <- readLn
  abs <- replicateM (n-1) $ do 
    [a,b] <- map read . words <$> getLine
    return (a,b)
  
  let ans = solve n abs
  print ans
         
