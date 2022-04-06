-- ttps://atcoder.jp/contests/typical90/submissions/30760253
solve :: Int -> Int -> Int -> [Int] -> Int
solve k l r as
  | r - l <= 1            = l
  | check k m as == True  = solve k m r as
  | check k m as == False = solve k l m as 
  where
    m = (l+r) `div` 2
    
check :: Int -> Int -> [Int] -> Bool
check k m as = count 0 0 as >= k + 1
  where
    count ct _ [] = ct
    count ct p (a:as)
      | a - p >= m = count (ct+1) a as
      | otherwise  = count ct p as


main = do
  [_,l] <- map read . words <$> getLine
  k <- readLn
  as <- map read . words <$> getLine :: IO [Int]

  let as' = as ++ [l]
  let ans = solve k 0 (l+1) as'
  print ans
