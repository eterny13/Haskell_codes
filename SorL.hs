comp :: Int -> Int -> IO()
comp a b = do
  if a == b then
    putStrLn "a == b"
  else if a > b then
    putStrLn "a > b"
  else 
    putStrLn "a < b" 
  

main = do
  let n1=2
  let n2=2
  comp n1 n2 
