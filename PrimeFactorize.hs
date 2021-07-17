-- https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=NTL_1_A

isPrime :: Int -> Bool
isPrime n = 
  null [y | y <- [2..floor $ sqrt $ fromIntegral n], n `mod` y == 0]

primeFactors :: Int -> [Int]
primeFactors n
  | isPrime n = [n]
  | otherwise = calc n [] 
  where
    calc 1 l = l
    calc n l = calc (n `div` a) (l ++ [a])
      where
        a = find n 
          where
            find n = [e | e <- [2..n], n `mod` e == 0] !! 0

main = do
  n <- readLn

  let ans = primeFactors n 
  --print ans
  putStrLn $ show n ++ ": " ++ ( unwords $ map show ans )
