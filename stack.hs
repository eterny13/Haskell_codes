charToOp :: Char -> Op
charToOp '+' = Plus
charToOp '-' = Minus
charToOp '*' = Multiply
charToOp c = error $ show c ++ " is not Operand"
 
data Op
  = Plus | Minus | Multiply  
  deriving Show

data Tree  
  = Leaf Int
  | Branch Op Tree Tree 
  deriving Show

calc :: Tree -> Int
calc (Leaf n) = n
calc (Branch op tr1 tr2) =
  case op of
    Plus     -> (calc tr1) + (calc tr2)
    Minus    -> (calc tr1) - (calc tr2) 
    Multiply -> (calc tr1) * (calc tr2) 

wordsToTree :: [String] -> Tree
wordsToTree (w1:w2:w3:_) = Branch op (Leaf n1) (Leaf n2) 
  where
    n1 = read w1
    n2 = read w2
    op = charToOp (head w3) 

judge :: String -> Tree
judge w3 = 
  case w3 of 
      

main = do
  print $ Leaf 1
  print $ Branch Plus (Leaf 1) (Leaf 2)
