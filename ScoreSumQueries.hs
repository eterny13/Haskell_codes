-- https://atcoder.jp/contests/typical90/submissions/32291276
import Data.Array
import Control.Monad

calc :: Array Int (Int,Int) -> (Int, Int) -> (Int, Int)
calc arr q = (at-as, bt-bs) 
  where
    (as,bs) = arr ! (-1 + fst q)
    (at,bt) = arr ! (snd q)

cumsum :: [(Int,Int)] -> [(Int,Int)]
cumsum ab = scanl (sump) (0,0) ab

sump :: (Int,Int) -> (Int,Int) -> (Int,Int)
sump (at, bt) (a,b) 
  | a == 1 = (at + b, bt)
  | a == 2 = (at, bt + b)
  | otherwise = (at, bt)



main = do
  n <- readLn
  ab <- replicateM n $ do
        [a,b] <- map read . words <$> getLine :: IO [Int]
        return (a,b)
  
  let arr = cumsum ab
  
  nq <- readLn 
  qs <- replicateM nq $ do
        [a,b] <- map read . words <$> getLine :: IO [Int]
        return (a,b)
        
  let ans = map (calc (listArray (0,n) arr)) qs

  forM_ ans $ \e -> do 
    putStrLn $ (show . fst) e ++ " " ++ (show . snd) e
    
