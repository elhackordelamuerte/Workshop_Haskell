-- 
-- EPITECH PROJECT, 2023
-- workshop-functional-prog
-- File description:
-- Mysum.hs
 --

mySum:: [Int] -> Int
mySum [] = 0
mySum (a:b) = a + mySum b