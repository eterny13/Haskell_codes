import Control.Monad
import Data.List

solve :: Int -> Int -> [(Int, Int)] -> Int 
solve l r hs 
  | abs(r - l) <= 1 = r 
  | otherwise = 
    if sch mid hs == True then solve l mid hs 
    else solve mid r hs
      where
        mid = (l + r) `div` 2 

sch :: Int -> [(Int, Int)] -> Bool
sch mid hs = jdg [0..(ln-1)] (sort hs')
  where 
    hs' = map (\(a,b) -> (fromIntegral (mid-a)) / (fromIntegral b)) hs
    ln = fromIntegral $ length hs'

jdg :: [Float] -> [Float] -> Bool
jdg [] _ = True
jdg _ [] = True
jdg (i:id) (h:hs) = if i > h then False else jdg id hs 

main = do
  n <- readLn 
  hs <- replicateM n $ do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    return (a,b) 
  
  let ans = solve 0 (10^20) hs
  print ans
