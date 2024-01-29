-- 
-- EPITECH PROJECT, 2023
-- workshop-functional-prog
-- File description:
-- Main.hs
 --
import System.Environment
import System.Exit
import Operation (addition, substract, multiply, divide)
import Text.Read
import Control.Monad
import Data.Char (isDigit)
import Parse (filterArr)


main::IO()
main = do
    input <- getLine
    let args = words input
    case () of
      _ | length args /= 3 -> putStrLn "Olala ça marche pas : <number1> <operator> <number2>" >> exitFailure
        | otherwise -> return ()

    let [s1, op, s2] = args
    case (readMaybe s1, readMaybe s2) of
        (Just n1, Just n2) -> case op of
            "+" -> print (addition n1 n2)
            "-" -> print (substract n1 n2)
            "*" -> print (multiply n1 n2)
            "/" -> if n2 == 0 then putStrLn "Error: Division by zero" else print (divide n1 n2)
            _   -> putStrLn "Error: Invalid operator"
        _ -> putStrLn "Error: Invalid number(s)"