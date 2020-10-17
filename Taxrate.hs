-- https://atcoder.jp/contests/sumitrust2019/tasks/sumitb2019_b
calc :: Int -> Int -> Int
calc d n  
  | n == n' = d 
  | n' < n = calc (d+1) n
  | otherwise = -1
  where
    n' = d * 108 `div` 100

main = do
  n <- readLn 

  let n' = n*100 
  let d = n' `div` 108
  let m = n' `mod` 108
  if m == 0 then
    print d

  else do
    let ans = calc d n 
    if ans < 0 then
      putStrLn ":("
    else
      print ans
