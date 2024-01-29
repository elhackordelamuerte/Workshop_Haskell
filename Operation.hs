-- 
-- EPITECH PROJECT, 2023
-- workshop-functional-prog
-- File description:
-- Operation.hs
 --
module Operation where

addition::Int -> Int -> Int
addition a b = a + b

substract::Int -> Int -> Int
substract a b = a - b

multiply::Int -> Int -> Int
multiply a b = a * b

divide::Int -> Int -> Maybe Int
divide a b = if b == 0 then Nothing else Just (a `div` b)

