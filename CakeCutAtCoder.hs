--https://atcoder.jp/contests/typical90/tasks/typical90_bx
import Control.Monad
import Data.Char
import Data.List
import Data.Array
import qualified Data.ByteString.Char8 as BS

readIntList :: IO [Int]
readIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

solve :: Int -> [Int] -> String
solve n as
  | even s == True && (or $ map (\a -> judge g a ass) ass) == True = "Yes"
  | otherwise       = "No"
  where
    s = sum as
    g = div s 10
    ass = scanl (+) 0 $ as ++ as

judge :: Int -> Int -> [Int] -> Bool
judge g a ass = bisearch 1 r g' ass' 
  where
    g' = a + g 
    r  = length ass
    ass' = listArray (1, r) ass

bisearch :: Int -> Int -> Int -> Array Int Int -> Bool
bisearch l r g ass 
  | r - l <= 1 = False
  | ass ! m < g = bisearch m r g ass
  | ass ! m > g = bisearch l m g ass
  | ass ! m == g = True 
  where
    m = div (l+r) 2

main = do
  n <- readLn
  as <- readIntList 

  putStrLn $ solve n as
  