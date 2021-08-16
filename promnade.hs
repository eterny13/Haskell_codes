-- https://atcoder.jp/contests/s8pc-1/tasks/s8pc_1_e
import Data.Array


calc :: [Int] -> [Int] -> [Int]
calc (x:[]) ds = ds
calc (x1:x2:xs) ds = calc (x2:xs) (ds ++ [d])
  where 
    d = modpow x1 x2 1000000007
    modpow x n m = go n
      where
        go 0 = 1
        go k
          | odd k     = (t * x) `mod` m
          | otherwise = t
            where
              s = go (k `div` 2)
              t = (s * s) `mod` m



distCalc :: Array Int Int -> [Int] -> Int -> Int
distCalc ds (p:[]) cnt = cnt
distCalc ds (p1:p2:ps) cnt = distCalc ds (p2:ps) cnt'
    where 
      cnt' = (cnt + (abs $ (ds ! (p1-1)) - (ds ! (p2-1)))) `mod` 1000000007

main = do
  [n,m] <- map read . words <$> getLine 
  a <- map read . words <$> getLine :: IO [Int]
  p <- map read . words <$> getLine :: IO [Int]

  let p' = (1:p) ++ [1]
  let distList = calc a []

  
  let distList' = zip [0..n-1] (scanl (+) 0 distList)

  let distArray = array (0, n-1) distList'
  let ans = distCalc distArray p' 0

  print ans
