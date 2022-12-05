import Control.Monad
import Data.List
import Data.Array
import qualified Data.Set as S

solve :: Int -> [[Int]] -> [(Int, Int)] -> Int
solve n as xys 
  | null order = -1
  | otherwise  = minimum t
  where
    xys' = S.fromList xys
    as' = listArray (1,n) $ map (listArray (1,n)) as
    order = [p | p <- permutations [1..n], isOK p ]
    isOK p = all (\(x,y) -> (x,y) `S.notMember` xys' && (y,x) `S.notMember` xys') $ zip p (tail p)
    t = map calct order
    calct p = sum $ zipWith (\x y -> as' ! x ! y) p [1..n]

main = do
  n <- readLn
  as <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  m <- readLn
  xys <- replicateM m $ do
          [x,y] <- map read . words <$> getLine :: IO [Int]
          return (x,y)
  
  print $ solve n as xys
