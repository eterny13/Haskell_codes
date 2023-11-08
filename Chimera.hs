--https://atcoder.jp/contests/typical90/tasks/typical90_bh
import Data.List
import qualified Data.Set as S
import Data.Maybe

solve :: [Int] -> Int
solve xs =  (maximum $ zipWith (+) lf lb) - 1
  where
    lf = lisLength xs
    lb = reverse $ lisLength $ reverse xs

lisLength :: [Int] -> [Int]
lisLength xs = ls
  where
    (_, ls) = mapAccumL calcLIS S.empty xs
  
calcLIS :: S.Set Int -> Int -> (S.Set Int, Int)
calcLIS acc x1 = (acc', l)
  where 
    mx = S.lookupGE x1 acc
    Just x2 = mx
    acc1 = S.insert x1 acc
    acc2 = S.delete x2 acc1 
    acc'  | isNothing mx = acc1
          | x2 == x1     = acc
          | otherwise    = acc2
    l = (S.findIndex x1 acc') + 1

main = do
  _ <- getLine 
  as <- map read . words <$> getLine :: IO [Int]

  let ans = solve as
  print ans
