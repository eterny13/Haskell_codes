-- AtCoder  https://atcoder.jp/contests/s8pc-6/tasks/s8pc_6_b 
import Control.Monad
import Control.Applicative

trav f t (a,b) = (abs(f-a)) + (abs(a-b)) + (abs(b-t))


main = do
  n <- readLn
  xs <- replicateM n $ do
    [a,b] <- map read . words <$> getLine :: IO [Int] 
    return (a,b)
  
  let from = fst (unzip xs)
  let to = snd (unzip xs)
  
  print $ minimum $ [sum $ map (trav f t) xs | f <- from, t <-to]

