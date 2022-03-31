-- https://atcoder.jp/contests/tenka1-2018-beginner/submissions/30593046
import Control.Monad

judge n is = filter (\i -> i * (i - 1) `div` 2 == n) is
cal n = n * (n + 1) `div` 2
    
solve k = map (\i -> (k-1): (makeset k i)) [0..k-1]
  where
    makeset :: Int -> Int -> [Int]
    makeset k i = [e+1..e+i] ++ map (\j -> 1 + i + (cal $ j - 1)) [i+1..k-1]
      where
        e = cal $ i - 1

main = do
  n <- readLn

  let res = judge n [1..1000]

  case null res of 
    True -> putStrLn "No"
    False -> do
      putStrLn "Yes"
      let k = head res 
      print k

      let ans = solve k 
      forM_ ans $ \as -> do
        putStrLn $ unwords $ map show as
    
