calc :: Int -> [Int] -> Bool
calc e as = all (\a -> f a) as  
  where
    f :: Int -> Bool
    f a = if t `mod` 10 == 5 then True else False
      where
        t =  e * 10 `div` a 


main = do
  [_, m] <- map read . words <$> getLine
  as <- map read . words <$> getLine :: IO [Int]

  let g = foldl (gcd) 2 as
  let l = foldl (lcm) 1 as

  let e = l `div` g
  let judge = if e <= m then calc e as else False
  let ans = (1 + m `div` e) `div` 2
  if judge == True then print ans else print 0
  
