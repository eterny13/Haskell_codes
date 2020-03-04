import Data.List

search :: [String] -> [Char] -> Int -> Int
search [] sd n = n
search (x:xs) sd n = search xs sd ct 
  where
    ct = if (isInfixOf x sd) == True then n+1 else n

main = do
  let a = "1 1 2 2 3"
  let b = "1 2"
  
  let ft = if (length a) <= (length b) then words a else words b
  let sd = if (length a) <= (length b) then b else a
  let ans = search ft sd 0
  print ans
    
    
