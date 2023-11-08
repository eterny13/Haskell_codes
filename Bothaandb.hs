--https://atcoder.jp/contests/typical90/tasks/typical90_bu
import Data.Array
import Control.Monad
import Control.Monad.ST
import Data.STRef

solve :: Int -> Array Int Char -> Array Int [Int] -> Int
solve n cs g = rec 1 (-1) False g cs memo
  where
    memo = listArray (1, n*2) $ replicate (n*2) (-1)

rec :: Int -> Int -> Bool -> Array Int [Int] -> Array Int Char -> Array Int Int -> Int
rec v p single g cs memo
  | memo ! idx /= -1  = memo ! idx
  | otherwise         = ans
  where
    idx = v * 2 + (if single == True then 1 else 0) 
    ans = runST $ do
      sans <- newSTRef 0
      let cv = cs ! v
      forM (g ! v) $ \x -> do
        let cx = cs ! x 
        when (x /= p) $ do 
          modifySTRef sans (+1)
        when (cv == cx) $ do 
          modifySTRef sans (+1) 
      readSTRef sans

main = do
  n <- readLn
  cs <- map read . words <$> getLine :: IO [Char]
  abs <- replicateM (n-1) $ do
        [a,b] <- map read . words <$> getLine :: IO [Int]
        return (a, b)
  
  let cs' = listArray (1, n) cs
  let g = accumArray (flip (:)) [] (1, n) $ concatMap (\(u,v) -> [(u,v), (v,u)]) abs
  let ans = solve n cs' g
  print ans
