range :: Int -> Int -> Int -> IO()
range a b c = do
  if a < b && b < c then
    putStrLn "Yes"
  else 
    putStrLn "No"

main = do
  let n1=1
  let n2=3
  let n3=2
  range n1 n2 n3
