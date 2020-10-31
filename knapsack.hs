import Control.Monad
import Data.Array

calc :: Int -> Int -> Array Int Int -> Array Int Int -> Array (Int, Int) Int
calc numn numw vs ws = dp 
  where
    dp = listArray ((0, 0),(numn, numw)) [loop i j | i <- [0..numn], j <- [0..numw]]

    loop :: Int -> Int -> Int
    loop 0 _ = 0
    loop _ 0 = 0
    loop i j 
      | w <= j    = max (dp ! (i-1,j)) (v + dp ! (i-1,j-w)) 
      | otherwise = dp ! (i-1,j)
      where
        v = vs ! (i-1)
        w = ws ! (i-1)
      

main = do

  [numn,numw] <- map read . words <$> getLine 

  gs <- replicateM numn $ do
        [v,w] <- map read . words <$> getLine :: IO [Int] 
        return (v,w)

  let vs = listArray (0, numn-1) $ map fst gs 
  let ws = listArray (0, numn-1) $ map snd gs 

  let dp = calc numn numw vs ws

  print $ dp ! (numn,numw)

