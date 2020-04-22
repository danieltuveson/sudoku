module Lib(solveLine) where

import Parse
import Solve

solveLine :: String -> String 
solveLine str = 
    case parse str of 
        Left err   -> err
        Right grid -> 
            case getSolution grid of 
                Nothing       -> "invalid sudoku, no solution exists."
                Just solution -> "sudoku solved!\n" ++ show solution
