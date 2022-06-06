-- https://atcoder.jp/contests/typical90/submissions/32291276
import Data.Array
import Control.Monad

calc :: Array Int (Int,Int) -> (Int, Int) -> (Int, Int)
calc arr q = (at-as, bt-bs) 
  where
    (as,bs) = arr ! (-1 + fst q)
    (at,bt) = arr ! (snd q)

cumsum :: Array Int (Int,Int) -> (Int,Int) -> Int -> (Int,Int)
cumsum arr pt i 
  | (fst $ arr ! i) == 1 = (at + (snd $ arr ! i), bt)
  | (fst $ arr ! i) == 2 = (at, bt + (snd $ arr ! i))
  | otherwise = (at, bt)
    where
      at = fst pt
      bt = snd pt

main = do
  n <- readLn
  ab <- replicateM n $ do
        [a,b] <- map read . words <$> getLine :: IO [Int]
        return (a,b)
  
  let arr = scanl (cumsum (listArray (0, n-1) ab)) (0,0) [0..n-1]
  
  nq <- readLn 
  qs <- replicateM nq $ do
        [a,b] <- map read . words <$> getLine :: IO [Int]
        return (a,b)
        
  let ans = map (calc (listArray (0,n) arr)) qs

  forM_ ans $ \e -> do 
    putStrLn $ (show . fst) e ++ " " ++ (show . snd) e
    
