import Control.Monad
import Data.Bits


comb :: Int -> [Int] -> [[Int]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs

func :: [(Int,Int)] -> [[Int]] -> [Int] -> Int
func xy ns bt = maximum $ map (\n -> calc n xy ns) bt 

calc :: Int -> [(Int,Int)] -> [[Int]] -> Int
calc n xy [] = popCount n
calc n xy ((a:b:xs):ns) = 
  if (shift n (-a)) `mod` 2 == 1 && (shift n (-b)) `mod` 2 == 1 && ((a,b) `elem` xy) == False then 
      -1
  else
    calc n xy ns
  
 
main = do
  [n,m] <- map read . words <$> getLine
  xy <- replicateM m $ do  
    [a,b] <- map read . words <$> getLine :: IO [Int]
    return (a-1, b-1)
  
  let ns = comb 2 [0..n-1]
  let ans = func xy ns [0..2^n-1]
  print ans
