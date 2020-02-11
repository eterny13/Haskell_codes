bsort :: [Int] -> [Int]
bsort [] = []
bsort xs = y : bsort ys
  where
    (y:ys) = swap xs 

swap :: [Int] -> [Int]
swap [x] = [x]
swap (x:xs)
  | x > y = y:x:ys
  | otherwise = x:y:ys 
  where 
    (y:ys) = swap xs

main = do
  let ans = bsort [5,3,2,4,1]
  print ans
