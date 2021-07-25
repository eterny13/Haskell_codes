-- https://atcoder.jp/contests/abc084/submissions/24528691
import Control.Monad
import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M

solve :: M.IntMap Int -> (Int, Int) -> Int
solve table lr = table M.! r - table M.! (l - 1)  
  where
    l = fst lr
    r = snd lr

makemap :: Int -> M.IntMap Int
makemap n = M.fromList $ scanl countp (0,0) [1..n]
  where
    primes = makeplist n
    countp (n, ct) i
      | odd i = if checkPrime i then (i, ct+1) else (i ,ct)
      | otherwise = (i, ct)
    checkPrime i = S.member i primes && S.member ((i + 1) `div` 2) primes


makeplist :: Int -> S.IntSet
makeplist n = S.filter (\i -> isPrime i == True) $ S.fromList (2 : [3,5..n])
  where
    isPrime :: Int -> Bool
    isPrime n = null [x | x <- [2 .. floor $ sqrt $ fromIntegral n], n `mod` x == 0 ] 

main = do
  q <- readLn
  lr <- replicateM q $ do
        [a,b] <- map read . words <$> getLine :: IO [Int]
        return (a,b)
  
  let table = makemap 100001
  let ans = map (solve table) lr
  forM_ ans $ \i -> do
    print i
