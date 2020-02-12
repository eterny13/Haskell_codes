chToElem :: Char -> RpnElem
chToElem '+' = Add
chToElem '-' = Subtract
chToElem '*' = Multiply
chToElem '/' = Divide

data RpnElem = Number Int
  | Add
  | Subtract
  | Multiply
  | Divide
  deriving Show


mkrpn :: [String] -> [RpnElem]
mkrpn [] = []
mkrpn (x:xs) = 
    case (head x) of
      '+' -> chToElem (head x) : (mkrpn xs)
      '-' -> chToElem (head x) : (mkrpn xs)
      '*' -> chToElem (head x) : (mkrpn xs)
      '/' -> chToElem (head x) : (mkrpn xs)
      otherwise -> (Number (read x :: Int)) : (mkrpn xs)
     
calcrpn :: [RpnElem] -> Int
calcrpn = head . foldl calcrpn' []

calcrpn' :: [Int] -> RpnElem -> [Int]
calcrpn' (x:y:xs)   Add       = (y+x:xs)
calcrpn' (x:y:xs)   Subtract  = (y-x:xs)
calcrpn' (x:y:xs)   Multiply  = (y*x:xs)
calcrpn' (x:y:xs)   Divide    = (y `div` x:xs)
calcrpn' xs       (Number x)  = (x:xs)  

main = do
  let state = mkrpn (words "1 2 +")  
  let ans = calcrpn state
  print ans
