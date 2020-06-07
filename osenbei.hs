import Control.Monad
import Data.Bits

solve :: Int -> Int -> [[Int]] -> Int
solve r c mas = maximum [ sum $ map (chmax r) $ schc (replicate c 0) (schr rn mas 0) | rn <- [0..2^r-1] ]

schr :: Int -> [[Int]] -> Int -> [[Int]] 
schr _ [] _ = []
schr rn (m:ms) ct = 
  if (shift rn (-ct)) `mod` 2 == 1 then 
    (rev: (schr rn ms (ct+1)) )
  else 
    (m : (schr rn ms (ct+1)) )
      where
        f n = if n == 1 then 0 else 1
        rev = map f m        


schc :: [Int] -> [[Int]] -> [Int]
schc zs [] = zs 
schc zs (m:ms) = schc (zipWith (+) zs m) ms  

chmax :: Int -> Int -> Int
chmax r n = max n (r-n)  

main = do
  [r,c] <- map read .words <$> getLine
  mas <- replicateM r $ map read . words <$> getLine :: IO [[Int]]
  
  let ans = solve r c mas 
  print ans
