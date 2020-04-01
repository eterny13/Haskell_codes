import Control.Monad
import Control.Applicative
import Text.Printf

calc :: Int -> Int -> Int -> [Int] -> Int
calc k max sum num = 
  if max == sum then max
  else
    if res <= k then calc k max ave num else calc k (ave+1) sum num
      where 
        ave = (max + sum) `div` 2 
        res = judge num ave 0 1

judge :: [Int] -> Int -> Int -> Int -> Int
judge [] ave sm ct = ct
judge (x:xs) ave sm ct = 
  if sm+x <= ave then judge xs ave (sm+x) ct 
  else judge xs ave x (ct+1)

main = do
  [n,k] <- map read . words <$> getLine
  num <- replicateM n readLn

  let max = maximum num
  let sum = foldl (+) 0 num
  let ans = calc k max sum num
  print ans
