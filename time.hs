time :: Int -> IO()
time s = do
  putStrLn $ show (s `div` 3600) ++ ":" ++ show ((mod s 3600) `div` 60) ++ ":" ++ show (mod s 60)

main = do
  let s = 46979
  time s
  
