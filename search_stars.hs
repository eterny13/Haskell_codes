-- AtCoder  https://atcoder.jp/contests/joi2008yo/tasks/joi2008yo_d
import Control.Monad
import Text.Printf

calc hd [] = []
calc hd (a:ast) =  
  ((fst a) - (fst hd), (snd a) - (snd hd)) : calc hd ast

calc' st ast (d:dif) =
  if (f st ast d) == True then d
  else calc' st ast dif
    
    
f [] ast d = True
f ((x,y):st) ast (x',y') =
  if ((x+x', y+y') `elem` ast) == True then
    f st ast (x',y')
  else 
    False

main = do
  m <- readLn
  st <- replicateM m $ do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    return (a,b)
  
  n <- readLn
  ast <- replicateM n $ do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    return (a,b)
  
  let dif = calc (head st) ast
  let ans = calc' st ast dif
  
  printf "%d %d\n" (fst ans) (snd ans)
