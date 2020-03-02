import Text.Printf
import Control.Monad

data Elmkey =
  Insert Int | Delete Int | DeleteFirst | DeleteLast 
  deriving Show

calc :: [Elmkey] -> [Int] -> [Int]
calc [] ls = ls
calc (x:xs) ls =
  case x of
    (Insert n) -> calc xs (n:ls)
    (Delete n) -> calc xs (filter (\t -> t /= n) ls)
    DeleteFirst -> calc xs (tail ls)
    DeleteLast  -> calc xs (filter (\t -> t /= (last ls)) ls)


main = do
  -- let fmt = chTokey ["insert 5", "insert 2","insert 3", "insert 1", "delete 3, insert 6, delete 5]
  let ans = calc [Insert 5, Insert 2, Insert 3, Insert 1, Delete 3, Insert 6, Delete 5] []
  forM_ ans $ \n -> do
    printf "%d " n
  printf "\n"
    
