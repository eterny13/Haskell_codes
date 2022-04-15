-- https://atcoder.jp/contests/typical90/submissions/30949880
import Data.Bits
import Control.Monad

makeStr n = filter (judge 0) sts
  where
    is = [0 .. (shift 1 n)] 
    js = [n-1, n-2 .. 0]
    sts = make is js
    make :: [Int] -> [Int] -> [String]
    make [] _ = []
    make (i:is) js = map (\j -> if (.&.) i (shift 1 j) == 0 then '(' else ')') js:(make is js)


judge d [] = d == 0
judge d (s:st) 
  | d < 0 = False
  | s == '(' = judge (d+1) st
  | otherwise = judge (d-1) st

main = do
  n <- readLn

  let str = makeStr n 

  forM_ str $ \s -> do
    putStrLn s
