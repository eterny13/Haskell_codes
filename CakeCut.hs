-- https://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=6371941#1
import Control.Monad
import Data.List
import Data.Ord
import qualified Data.Vector as V

data Cake = Cake Int Int deriving (Show, Eq, Ord)

class Cutting a where
  dep  :: a -> Int
  wid  :: a -> Int
  size :: a -> Int
  cut  :: a -> Int -> (a,a)
  comp :: a -> a -> (a,a) 

instance Cutting Cake where
  dep  (Cake a b) = a 
  wid  (Cake a b) = b
  size (Cake a b) = a * b
  cut  (Cake a b) s
    | s' > a    = comp (Cake a (s'-a)) (Cake a (b-(s'-a)))
    | otherwise = comp (Cake s' b) (Cake (a-s') b)
    where
      wd = a + b
      s' = s `mod` wd
  comp c1 c2
    | size c1 < size c2 = (c1,c2)
    | otherwise = (c2,c1)


solve :: [(Int,Int)] -> V.Vector Cake -> [Cake] 
solve [] cs = sortBy (comparing size) $ V.toList cs
solve (q:qs) cs = solve qs $ V.snoc (V.snoc (V.fromList cs') c1) c2
  where
    p = -1 + fst q 
    s = snd q 
    cs' = remove p $ V.toList cs
    c = V.unsafeIndex cs p
    (c1, c2) = cut c s

remove :: Int -> [a] -> [a]
remove i xs = let (f,s) = splitAt (i+1) xs in init f ++ s

input = do
  [n,w,d] <- map read . words <$> getLine
  if n + w + d /= 0
    then do
      qs <- replicateM n $ do
        [p,s] <- map read . words <$> getLine :: IO [Int]
        return (p,s) 

      let ans = map (size) $ solve qs (V.fromList [Cake w d])
      putStrLn $ unwords $ map show ans
      input
    else do
      return ()


main = do
  input
