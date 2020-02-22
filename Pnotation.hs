data Pelm =
    Number Int
  | Add
  | Subtract
  | Multiply
  | Divide
  deriving Show

chToOp :: [String] -> [Pelm]
chToOp [] = []
chToOp (x:xs) = 
  case (head x) of
    '+' -> Add : chToOp xs
    '-' -> Subtract : chToOp xs
    '*' -> Multiply : chToOp xs
    '/' -> Divide : chToOp xs
    otherwise -> (Number (read x :: Int)) : chToOp xs

calc :: [Pelm] -> Int
calc = head . foldl calc' []

calc' :: [Int] -> Pelm -> [Int]
calc' (x:y:xs) Add      = (y+x:xs)
calc' (x:y:xs) Subtract = (y-x:xs)
calc' (x:y:xs) Multiply = (y*x:xs)
calc' (x:y:xs) Divide   = (y`div`x:xs)
calc' xs (Number x)     = (x:xs)

main = do
  let state = chToOp (words "1 2 +")
  print state
  let ans = calc state
  print ans
