import Control.Monad
import Text.Printf
-- import Data.Maybe
-- import qualified Data.ByteString.Char8 as B

toInt [] = []  
toInt (x:xs) = (read x :: Int) : toInt xs

bsort :: Int -> [Int] -> (Int, [Int])
bsort ct [] = (ct, [])
bsort ct [x] = (ct, [x])
bsort ct (x:xs) = f y $ (bsort ct' ys)
  where
    (ct', (y:ys)) = bswap ct (x:xs)
    f a (b, c) = (b, (a:c))

bswap ct [x] = (ct,[x])
bswap ct (x:xs)
    | x > y     =  (ct'+1, (y:x:ys))
    | otherwise =  (ct', (x:y:ys))
    where
      (ct', (y:ys)) = bswap ct xs

-- readIntList :: IO [Int]
-- readIntList = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

main = do
  a <- getLine
  -- s <- readIntList
  s <- getLine
  let tlist = toInt (words s)
  let (n, ans) = bsort 0 tlist 

  --print "answer"
  print n
  
  --forM_ ans $ \i -> do
    --printf "%d\n" i

