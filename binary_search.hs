-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ALDS1_4_B
-- This code cannot solve a last data set
import Data.List

solve :: [Int] -> [Int] -> Int
solve ss ts = length $ filter (==True) $ map (\i -> judge ss i) ts
  where 
    judge ss i = 
      if (maximum ss) < i || (minimum ss) > i then False
      else bsearch 0 (length ss) ss i

bsearch :: Int -> Int -> [Int] -> Int -> Bool
bsearch l r ss v 
  | abs(l-r) <= 1 = if (ss !! l) == v || (ss !! r) == v then True else False
  | otherwise = 
    if v == e then
      True
    else if v < e then
      bsearch l mid ss v
    else 
      bsearch mid r ss v
      where
          mid = (l+r) `div` 2
          e = ss !! mid
  
readInt :: String -> Int
readInt = read

has :: (Eq a) => [a] -> a -> Bool
has [] _ = False
has (x:xs) a
  | x == a    = True
  | otherwise = has xs a

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | has xs x  = unique xs
  | otherwise = x : unique xs

main = do
  n <- getLine
  s <- fmap (map readInt . words) getLine 

  q <- getLine 
  t <- fmap (map readInt . words) getLine 

  let ans = solve (unique $ sort s) (sort t) 
  print ans
