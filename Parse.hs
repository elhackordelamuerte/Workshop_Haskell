--
-- EPITECH PROJECT, 2023
-- workshop-functional-prog
-- File description:
-- Parse.hs
 --

module Parse where
import Data.Char (isDigit)

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (a:b)
    |x == a = True
    |otherwise = myElem x b

isOperator :: String -> Bool
isOperator s = myElem s ["+", "-", "*", "/"]

isNumber :: String -> Bool
isNumber = all isDigit

filterArr :: (a -> Bool) -> [a] -> [a]
filterArr_ [] = []
filterArr p (x:xs)
    | p x = x : filterArr p xs
    | otherwise = filterArr p xs

parseLine :: [String] -> ([String], [String])
parseLine xs = (filterArr isNumber xs, filterArr isOperator xs)

