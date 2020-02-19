queue :: Int -> Int -> [([Char], Int)] ->  [([Char], Int)]
queue t q [] = []
queue t q xs = y: queue (snd y) q ys
  where 
    y:ys = queue' t q xs

queue' :: Int -> Int -> [([Char], Int)] -> [([Char], Int)]
queue' t q ((prc, 0):xs) = [(prc, t)] ++ xs
queue' t q (x:xs) =  
  if time > 0 then
    queue' (t+q) q (xs ++ [((fst x), time)] )
  else
    queue' (t+(snd x)) q ([((fst x), 0)] ++ xs) 
    where
      time = (snd x) - q



main = do
  let ans = queue 0 100 [("p1", 150),("p2", 80),("p3", 200),("p4", 350),("p5", 20)] 
  mapM_ print ans
  
