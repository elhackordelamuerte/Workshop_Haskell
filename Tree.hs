module Tree where
data ASTree = Add ASTree ASTree
            | Sub ASTree ASTree
            | Mul ASTree ASTree
            | Div ASTree ASTree
            | Value Int
instance Show ASTree where
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2
    show (Div e1 e2) = show e1 ++ " / " ++ show e2
    show (Value n)   = show n

buildExpr :: ASTree -> String -> ASTree -> ASTree
buildExpr tree1 operation tree2 = case operation of
    "+" -> Add tree1 tree2
    "-" -> Sub tree1 tree2
    "*" -> Mul tree1 tree2
    "/" -> Div tree1 tree2
    _   -> error "Invalid operation"

mapValues :: [String] -> [ASTree]
mapValues = map (Value . read)