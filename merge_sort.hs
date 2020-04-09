import Control.Monad

merge_sort :: [Int] -> [Int]  
merge_sort [] = []
merge_sort [x] = [x]
merge_sort xs = merge (merge_sort left) (merge_sort right)
  where
    left = take half xs
    right = drop half xs
    half = length xs `div` 2

merge :: [Int] -> [Int] -> [Int]
merge ls [] = ls
merge [] rs = rs
merge (l:ls) (r:rs) 
  | l <= r = l : merge ls (r:rs)
  | otherwise = r : merge (l:ls) rs

main = do
  n <- readLn
  list <- replicateM n readLn
  print $ merge_sort list 

