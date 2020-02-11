import Data.List

sl_sort :: [Int] -> [Int]
sl_sort [] = []
sl_sort xs = y : sl_sort ys
  where
    (y:ys) = swap xs

swap :: [Int] -> [Int]
swap [] = [] 
swap xs = mn:(delete mn xs)
  where 
    mn = minimum xs
      
main = do
  let ans = sl_sort [1,5,3,2,4,1]
  print ans
