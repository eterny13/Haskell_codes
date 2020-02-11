ins_sort :: Ord a => [a] -> [a]
ins_sort xs = ins_sort' 1 (tail xs) xs where 
  ins_sort' :: Ord a => Int -> [a] -> [a] -> [a]
  ins_sort' i [] xs = xs
  ins_sort' i (k:ks) xs = ins_sort' (i+1) (ks) (f xs') 
  f :: Ord a => [a] -> Int -> a -> [a] 
  f xs idx e = 
    where
      (xs', j) = loop (i-1) xs
      loop :: Ord a => Int -> [a] -> ([a], Int) 
      loop j ys 
        | j < 0 || aj <= k = (ys, j)    
        | otherwise = loop j' ys'
        where
          ys' = undefined
          j' = j - 1  

main = do
  let nList = [5,2,4,6,1,3]
  ins_sort nList
