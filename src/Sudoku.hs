module Sudoku where

import Parse
import Solve
import Grid(Grid, showGrid)

solveLine :: String -> String 
solveLine str = 
    case getGridSolution str of 
        Left err       -> err
        Right solution -> "sudoku solved!\n" ++ showGrid solution

getGridSolution :: String -> Either String Grid
getGridSolution str = 
    case parse str of 
        Left err   -> Left err
        Right grid -> 
            case getSolution grid of 
                Nothing       -> Left "invalid sudoku, no solution exists."
                Just solution -> Right solution