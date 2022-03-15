-- https://atcoder.jp/contests/abc150/submissions/30151129
calc :: Int -> [Int] -> Bool
calc l as = all (\a -> f a) as  
  where
    f :: Int -> Bool
    f a = if t `mod` 2 /= 0 then True else False
      where
        t = l `div` a


main = do
  [_, m] <- map read . words <$> getLine
  as <- map read . words <$> getLine :: IO [Int]

  let as' = map (`div` 2) as
  let l = foldl (lcm) 1 as'

  let ans = if (calc l as') == True then (1 + m `div` l) `div` 2 else 0
  print ans
