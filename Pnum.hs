import Data.List

cList :: [Int] -> Int
cList xs = pnum (nub xs) 0 where 
  pnum :: [Int] -> Int -> Int
  pnum [] ct = ct
  pnum (l:ls) ct 
    | l <= 0 = error "N >= 1"
    | l == 1 = pnum ls ct
    | l == 2 = pnum ls (ct+1)
    | l `mod` 2 == 0 = pnum ls ct
    | otherwise  =  ela l 3
    where
      ela :: Int -> Int -> Int
      ela l' i 
        | l' `mod` i == 0 = pnum ls ct
        | l' < i^2 = pnum ls (ct+1)
        | otherwise = ela l' (i+2) 

main = do
  let n = cList [11,7,11,13,15,17]
  print n

