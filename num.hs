pnum :: Int -> Int -> Int
pnum n ct = 
  if n `mod` 2 == 0 then 
    ct+1 
  else 
    ct

main = do
  let ans = pnum 5 0
  print ans
