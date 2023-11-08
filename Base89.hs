--https://atcoder.jp/contests/typical90/tasks/typical90_bo
import Numeric
import Data.Char

calc :: Integer -> Integer -> Integer
calc n 0 = n
calc n k = calc nxt (k-1)
  where
    tba = fst $ head $ readOct $ show n
    nba = showIntAtBase 9 intToDigit tba ""
    nxt = read ( map (\i -> if i == '8' then '5' else i) nba ) :: Integer

main = do
  [n,k] <- map read . words <$> getLine :: IO [Integer]

  let ans = calc n k
  print ans