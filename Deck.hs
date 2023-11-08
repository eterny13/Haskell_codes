--https://atcoder.jp/contests/typical90/tasks/typical90_bi
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List
import qualified Data.Sequence as Sq

solve :: [(Int,Int)] -> Sq.Seq Int ->  [Int] -> [Int]
solve [] _ as = reverse as
solve (p:ps) seq as 
  | fst p == 1 = solve ps (x Sq.<| seq) as
  | fst p == 2 = solve ps (seq Sq.|> x) as
  | fst p == 3 = solve ps seq ((Sq.index seq (pred x)):as)
  where
    x = snd p
    

main = do
  q <- readLn

  tx <- replicateM q $ do
    [t,x] <- map  read . words <$> getLine :: IO [Int]
    return (t,x)
  
  let ans = solve tx Sq.empty []
  forM_ ans $ \a -> do
    print a