-- https://atcoder.jp/contests/abc084/submissions/24510897
import Control.Monad
import qualified Data.Set as S
 
solve :: (Int, Int) -> Int
solve lr = S.size $ S.filter (\i -> isPrime' i == True) $ S.filter (\i -> l<= i && i <= r) plst 
  where
    l = fst lr
    r = snd lr
    plst = S.filter (\i -> isPrime i == True) $ S.fromList [2 .. r] 
    isPrime' n = S.member n' plst 
      where n' = (n + 1) `div` 2
 
 
isPrime :: Int -> Bool
isPrime n = null [x | x <- [2 .. floor $ sqrt $ fromIntegral n], n `mod` x == 0 && n /= 1 ]
 
main = do
  q <- readLn
  lr <- replicateM q $ do
        [a,b] <- map read . words <$> getLine :: IO [Int]
        return (a,b)
  
  let ans = map solve lr
  forM_ ans $ \i -> do
    print i
