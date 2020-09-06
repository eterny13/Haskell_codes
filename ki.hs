import Data.List
import Data.Array
import Data.Char
import Control.Monad

solve :: Int -> Int -> [(Int, Int)] -> [(Int,Int)] -> [Int]
solve n q ab p = ans
  where
    g = accumArray (flip (:)) [] (1,n) [p | (a,b) <- ab, p <- [(a,b),(b,a)]]
    ps = accumArray (+) 0 (1,n) p
    ans = elems $ array (1,n) $ calc 0 0 1 []
    calc u v id li = (id, v') : foldr (calc id v') li (delete u $ g ! id)
      where
        v' = v + ps ! id

main = do
  [n,q] <- map read . words <$> getLine
  tr <- replicateM (n-1) $ do 
          [a,b] <- map read . words <$> getLine :: IO [Int]
          return (a,b)
  p <- replicateM q $ do
          [a,b] <- map read . words <$> getLine :: IO [Int]
          return (a,b)

  let ans = solve n q tr p 
  putStrLn $ unwords $ map show ans
