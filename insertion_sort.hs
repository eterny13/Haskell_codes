import Data.List

ins_sort :: (Ord a) => [a] -> [a]
ins_sort = foldr insert []

main = do
  let ans = ins_sort [5,2,3,4,1]
  print ans
