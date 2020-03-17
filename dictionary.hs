import Control.Monad
import Text.Printf

data Dictkey = 
    Insert String 
  | Find String
  deriving Show

chToDict x =
  case (head x) of
    'i'  -> Insert (last (words x)) 
    'f'  -> Find (last (words x))
    

finding :: [Dictkey] -> [String] -> [String] -> [String]
finding [] st yn = yn 
finding (x:xs) st yn =
  case x of
    (Insert str)  -> finding xs (str:st) yn
    (Find str)    -> finding xs st ((chk str st):yn)

chk :: String -> [String] -> String
chk str st = yn'
  where
    yn' = if (elem str st) == True then "yes" else "no"


main = do
  n <- readLn 
  state <- replicateM n getLine
  let diclist = map chToDict state
  -- print diclist
  
  let ans = finding diclist [] []
  -- print (reverse ans)
  forM_ (reverse ans) $ \i -> do
    printf "%s\n" i
