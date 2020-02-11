profit :: [Int] -> Int -> Int -> Int
profit [] maxp minp = maxp
profit (x:xs) maxp minp = profit xs maxp' minp'
  where
    maxp' = if maxp < x-minp then x-minp else maxp
    minp' = if minp > x then x else minp

main = do
  let ans = profit [5,3,1,3,4,3] (-1000) 1000
  print ans
