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

buildExprList :: [String] -> [ASTree] -> ASTree
buildExprList [] [tree] = tree
buildExprList (op:ops) (t1:t2:trees) = buildExprList ops ((buildExpr t1 op t2) : trees)
buildExprList _ _ = error "Invalid input"

buildAST :: ([String], [String]) -> ASTree
buildAST (ops, values) = buildExprList ops (mapValues values)

evalall :: ASTree -> Int
evalall (Value n) = n
evalall (Add e1 e2) = evalall e1 + evalall e2
evalall (Sub e1 e2) = evalall e1 - evalall e2
evalall (Mul e1 e2) = evalall e1 * evalall e2
evalall (Div e1 e2) = evalall e1 `div` evalall e2